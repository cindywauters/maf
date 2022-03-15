package maf.language.scheme

import maf.core.*
import maf.core.Label
import maf.language.change.ChangeExp
import maf.modular.incremental.IncrementalModAnalysis
import maf.core.Expression
import maf.modular.incremental.update.{IncrementalModAnalysisWithUpdateTwoVersions, IncrementalUpdateDatastructures}
import maf.modular.scheme.modf.SchemeModFComponent

import scala.::
import scala.annotation.tailrec

type ReanalysisList     = List[(Option[Expression], Option[Expression])]
type RenamingsList      = List[((Expression, Expression), (Boolean, Map[Identifier, Identifier]))]
type IfsList            = List[((SchemeIf, SchemeIf),  List[Identifier], (Expression, Expression))]
type BindingsWithScopes = Map[Expression, (Identifier, Map[String, Identifier])]
type BindingTuples      = (Expression, (Identifier, Map[String, Identifier]))
type ScopeChanges       = Map[BindingTuples, BindingTuples]

class SchemeChangePatterns:

  var reanalyse: ReanalysisList = List()
  var rename: RenamingsList = List()
  var needed_prims: List[Identifier] = List()
  var ifs: IfsList = List()
  var inserts: List[Expression] = List()
  var deletes: List[Expression] = List()
  var scopeChanges: ScopeChanges = Map()
  var maybeReanalyse: Map[Expression, (Option[Expression], Option[Expression])] = Map()
  var renamedBindingsIds: Map[Identifier, Identifier] = Map()

// find all the changed expressions and create a set with all the old and new expressions
  def findAllChangedExpressions(expr: Expression): Set[ChangeExp[Expression]] = expr match
    case e: ChangeExp[Expression] => Set(e) // Assumption: change expressions are not nested.
    case e                        =>  e.subexpressions.asInstanceOf[List[Expression]].flatMap(findAllChangedExpressions).toSet


// Get a list of all the variables that are declared within an expression (through lambdas, let, letrec, or let*)
  def findAllVarsInOrder(exp: Expression): List[Identifier] = exp match
    // In case of lambda, add the arguments and look at the body for more
    case SchemeLambda(name, args, body, annotation, pos) =>
      args.map(arg => arg).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
    // In case of let, let* and letrec, take the names of the bindings and look in the body for more
    case SchemeLet(bindings, body, pos) =>
      val bindingsVars = bindings.flatMap(binding => findAllVarsInOrder(binding._2))
      val bodyVars = body.flatMap(e => findAllVarsInOrder(e))
      bindings.map(binding => binding._1).appendedAll(bindingsVars).appendedAll(bodyVars)
    case SchemeLetStar(bindings, body, pos) =>
      val bindingsVars = bindings.flatMap(binding => findAllVarsInOrder(binding._2))
      val bodyVars = body.flatMap(e => findAllVarsInOrder(e))
      bindings.map(binding => binding._1).appendedAll(bindingsVars).appendedAll(bodyVars)
    case SchemeLetrec(bindings, body, pos) =>
      val bindingsVars = bindings.flatMap(binding => findAllVarsInOrder(binding._2))
      val bodyVars = body.flatMap(e => findAllVarsInOrder(e))
      bindings.map(binding => binding._1).appendedAll(bindingsVars).appendedAll(bodyVars)
    // In case of a list of expressions, look at each expression individually
    case exps: List[_] =>
      exps.flatMap(e => findAllVarsInOrder(exps))
    // In case of a single expression, look at the subexpressions (if there are any)
    case e: Expression =>
      if e.subexpressions.nonEmpty then
        e.subexpressions.flatMap(e => findAllVarsInOrder(e))
      else  List()
    case e: _ =>
      List()


  def checkRenamingsVariables(oldexp: Expression, newexp: Expression): (Boolean, Map[Identifier, Identifier]) =
    (oldexp, newexp) match
      case (oe: SchemeExp, ne: SchemeExp) =>
        val renamedOld = SchemeRenamer.rename(oe)
        val renamedNew = SchemeRenamer.rename(ne)
        val variablesOld = findAllVarsInOrder(oe)
        val variablesNew = findAllVarsInOrder(ne)
        val variablesRenamedOld = findAllVarsInOrder(renamedOld)
        val variablesRenamedNew = findAllVarsInOrder(renamedNew)
        if variablesOld.length != variablesNew.length then
          return (false, Map())
        var mappedVars = variablesNew.map(e => e.name).zip(variablesOld.map(e => e.name)).toMap
        val mappedRenamedVars = variablesRenamedNew.map(e => e.name).zip(variablesRenamedOld.map(e => e.name)).toMap
        val mappedIdentifiers = variablesNew.zip(variablesOld).toMap
        if renamedOld.eql(SchemeChangeRenamerForPatterns.rename(renamedNew, mappedRenamedVars, Map[String, Int]())._1) then
          return (true, mappedIdentifiers)
    return (false, Map())

  def checkForRenamingParameter(exp: SchemeExp): Set[((Expression, Expression), (Boolean, Map[Identifier, Identifier]))] =
    val changedExpr = findAllChangedExpressions(exp)
    val changedExprAsPairs = changedExpr.map(e => (e.old, e.nw))
    // toList necessary because sets cannot contain duplicate items
    val renamings: List[(Boolean, Map[Identifier, Identifier])] = changedExprAsPairs.toList.map(e => checkRenamingsVariables(e._1, e._2))
    changedExprAsPairs.zip(renamings)

  def compareRenamingsBindings(oldname: Identifier, newname: Identifier, oldexp: SchemeExp, nwexp: SchemeExp): (Boolean, Map[Identifier, Identifier]) =
    var mappedVars = Map(newname.name -> oldname.name)
    var mappedIds = Map(newname -> oldname)
    (oldexp, nwexp) match
      case (ol: SchemeLambda, nl: SchemeLambda) =>
        val varsOld = findAllVarsInOrder(ol)
        val varsNew = findAllVarsInOrder(nl)
        mappedVars = mappedVars ++ varsNew.map(_.name).zip(varsOld.map(_.name)).toMap
        mappedIds = mappedIds ++ varsNew.zip(varsOld).toMap
      case _ =>
    if oldexp.eql(SchemeChangeRenamerForPatterns.rename(nwexp, mappedVars, Map[String, Int]())._1) then
      (true, mappedIds)
    else
      (false, Map())

  // conditions are represented as a list as in case such as (not (...)) the arguments of not are used
  def comparePartialCondAndBranches(oldCond: List[SchemeExp], newCond: List[SchemeExp], oldTrue: SchemeExp, oldFalse: SchemeExp, newTrue: SchemeExp, newFalse: SchemeExp): Boolean =
    oldCond.zip(newCond).map(e => e._1.eql(e._2)).forall(e => e) && oldTrue.eql(newFalse) && newTrue.eql(oldFalse)

  def compareIfs(oldIf: SchemeIf, newIf: SchemeIf, prims: List[Identifier]): Option[(List[Identifier], (Expression, Expression))] =
    (oldIf.cond, newIf.cond) match
      case (oldFun: SchemeFuncall, newFun: SchemeFuncall) =>
        (oldFun.f, newFun.f) match
          case (SchemeVar(oldId), _) if oldId.name == "not" =>
            if comparePartialCondAndBranches(oldFun.args, List(newFun), oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
              Some(List(), (oldFun.args.head, newFun))
            else None
          case (_, SchemeVar(newId)) if newId.name == "not" =>
            if comparePartialCondAndBranches(List(oldFun), newFun.args, oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
              Some(prims.filter(e => e.name == "not"), (oldFun, newFun.args.head))
            else None
          case (SchemeVar(oldId), SchemeVar(newId)) =>
            if oldId.name == "<=" && newId.name == ">"|| oldId.name == ">" && newId.name == "<=" || oldId.name == ">=" && newId.name == "<" || oldId.name == "<" && newId.name == ">=" then
              if comparePartialCondAndBranches(oldFun.args, newFun.args, oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                return Some(prims.filter(e => e.name == newId.name), (oldFun, newFun))
            None
          case (_, _) =>
            None
      case (oldVal: SchemeValue, newFun: SchemeFuncall) =>
        newFun.f match
          case SchemeVar(newId) =>
            if newId.name == "not" then
              if comparePartialCondAndBranches(List(oldVal), newFun.args, oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                return Some(prims.filter(e => e.name == "not"), (oldVal, newFun.args.head))
            None
          case _ => None
      case (oldFun: SchemeFuncall, newVal: SchemeValue) =>
        oldFun.f match
          case SchemeVar(oldId) =>
            if oldId.name == "not" then
              if comparePartialCondAndBranches(oldFun.args, List(newVal), oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                return Some(List(), (oldFun.args.head, newVal))
            None
          case _ => None
      case (e1: _, e2: _) => None

  def findLowestChangedSubExpressions(old: Expression, nw: Expression): Unit =
    if old.subexpressions.isEmpty && nw.subexpressions.isEmpty then
      reanalyse = reanalyse.::((Some(old), Some(nw)))
    println("looking at")
    println(old)
    println(nw)
    (old, nw) match
      case (ol: SchemeLettishExp, nl: SchemeLettishExp) if ol.idn == nl.idn =>
        val renamed = checkRenamingsVariables(ol, nl)
        if renamed._1 then
          rename = rename.::((ol, nl), renamed)
          return
      case (ol: SchemeLambda, nl: SchemeLambda) =>
        val renamed = checkRenamingsVariables(ol, nl)
        if renamed._1 then
          rename = rename.::((ol, nl), renamed)
          return
      case (oe: Identifier, ne: Identifier) =>
        if renamedBindingsIds.contains(oe) then
          rename = rename.::((oe, ne), (true, Map(ne -> oe)))
          return
      case (oldIf: SchemeIf, newIf: SchemeIf) =>
        val maybeSwapped = compareIfs(oldIf, newIf, needed_prims)
        maybeSwapped match
          case Some((added, conds)) =>
            ifs = ifs.::((oldIf -> newIf), added, conds)
          case None =>
            if oldIf.eql(newIf) then
              rename = rename.::((oldIf, newIf), (true, Map()))
          return
      case _ =>
    var addToMaybe: List[Expression] = List()
    old.subexpressions.foreach(oe =>
      nw.subexpressions.find(ne => oe.idn == ne.idn) match
        case Some(ne) =>
          if oe.subexpressions.exists(oe => ne.subexpressions.exists(ne => oe.idn == ne.idn)) then
            findLowestChangedSubExpressions(oe, ne)
          else
            reanalyse = reanalyse.::((Some(oe), Some(ne)))
        case None =>
          deletes = deletes.::(oe)
          addToMaybe = addToMaybe.::(oe))
    nw.subexpressions.foreach(ne =>
      if !old.subexpressions.exists(oe => oe.idn == ne.idn) then
        println("inserting here")
        inserts = inserts.::(ne)
        addToMaybe = addToMaybe.::(ne))
    addToMaybe.foreach(maybe =>
      maybeReanalyse = maybeReanalyse + (maybe -> (Some(old), Some(nw))))


  // Returns a list of expressions that needs to be reanalysed (old and new), and a list of tuples of expressions that are just renamings together with their mappings
  def comparePrograms(old: SchemeExp, nw: SchemeExp, analysis: Option[IncrementalModAnalysisWithUpdateTwoVersions[_]] = None): differentChanges =
    lazy val allOldScopes = findLexicalScopes(old)
    lazy val allNewScopes = findLexicalScopes(nw)
    (old, nw) match
      case (oldlet: SchemeLettishExp, newlet: SchemeLettishExp) =>
        var relatedBindings: Map[Expression, Expression] = Map()
        oldlet.bindings.foreach(oe =>
          newlet.bindings.find(ne => (oe != ne) && (oe._2.idn == ne._2.idn)) match
            case Some(ne) =>
              val renamed = compareRenamingsBindings(oe._1, ne._1, oe._2, ne._2)
              if renamed._1 then
                rename = rename.::((oe._2, ne._2), renamed)
                renamedBindingsIds = renamedBindingsIds + (oe._1 -> ne._1)
              else
                relatedBindings = relatedBindings + (oe._2 -> ne._2)
            case _       =>
          if !newlet.bindings.exists(ne => oe._2.idn == ne._2.idn) then
            deletes = deletes.::(oe._2)
          if oe._1.idn.idn.tag != Position.noTag then
            needed_prims = needed_prims.::(oe._1))
        newlet.bindings.foreach(ne =>
          if !oldlet.bindings.exists(oe => oe._2.idn == ne._2.idn) then
            inserts = inserts.::(ne._2)
          if ne._1.idn.idn.tag != Position.noTag && !needed_prims.contains(ne._1) then
            needed_prims = needed_prims.::(ne._1))
        println("related bindings")
        println(relatedBindings.size)
        relatedBindings.foreach(related =>
          findLowestChangedSubExpressions(related._1, related._2))
        println("218")
        deletes.foreach(deleted =>
          inserts.find(i => i.eql(deleted)) match
            case Some(inserted) =>
              val oldEnv = allOldScopes.get(deleted)
              val newEnv = allNewScopes.get(inserted)
              println("deleted:")
              println(deleted)
              println("inserted:")
              println(inserted)
              (oldEnv, newEnv) match
                case (Some(oenv: (Identifier, Map[String, Identifier])), Some(nenv: (Identifier, Map[String, Identifier]))) =>
                  println("envs the same?")
                  println(inserted.idn.toString + " " + deleted.idn.toString)
                  println(oenv)
                  println(nenv)
                  if oenv._2 == nenv._2 && oenv._1.name == nenv._1.name then
                    scopeChanges = scopeChanges + ((deleted, oenv) -> (inserted, nenv))
                case _ =>
                  println("not found: ")
                  println(deleted)
                  println(inserted)
                  println(allOldScopes)
            case _ =>)
        println("maybes")
        println(maybeReanalyse)
        deletes.foreach(deleted =>
          println("looking at deleted")
          println(deleted)
          if !scopeChanges.exists(s => s._1._1 == deleted) then
            println(deleted)
            maybeReanalyse.find(r => r._1.eql(deleted)) match
              case Some(toReanalyse) => reanalyse = reanalyse.::(toReanalyse._2))
        inserts.foreach(inserted =>
          println("looking at inserted")
          println(inserted)
          if !scopeChanges.exists(s => s._2._1 == inserted) then
            println(inserted)
            maybeReanalyse.find(r => r._1.eql(inserted)) match
              case Some(toReanalyse) => reanalyse = reanalyse.::(toReanalyse._2))
        println("reanalyse")
        println(reanalyse)
        println("rename")
        println(rename.size)
        println("ifs")
        println(ifs)
        println("scope changes")
        println(scopeChanges)
        differentChanges(reanalyse, rename, ifs, scopeChanges)
      case _ => differentChanges(reanalyse, rename, ifs, scopeChanges)

  var up = new IncrementalUpdateDatastructures

  @tailrec
  final def findEquivalent(expr: Expression, in: List[Expression]): Option[Expression] = in match
    case List() => None
    case first :: _ if first.idn == expr.idn && first.getClass == expr.getClass => Some(first)
    case first :: second :: rest if first.idn.idn.line <= expr.idn.idn.line && expr.idn.idn.line < second.idn.idn.line =>
      val subs: List[Expression] =  up.findAllSubExps(first)
      findEquivalent(expr, subs.tail)
    case first :: rest => findEquivalent(expr, rest)

  def findEquivalentLambdas(lams: List[Expression], program: Expression): List[(Expression, Option[Expression])] =
    var matchingLams: List[(SchemeLambda, SchemeLambda)] = List()
    program match
      case let: SchemeLettishExp =>
        val bindings = let.bindings.map(_._2)
        lams.map(e => (e, findEquivalent(e, bindings)))

  def allBindingsInProgramIdSchemeExp(expr: Expression): List[(Identifier, Expression)] =
    if expr.subexpressions.isEmpty && expr.height == 1 then
      List()
    else if expr.subexpressions.isEmpty then
      List()
    else expr match
      case let: SchemeLettishExp => let.bindings.appendedAll(let.bindings.flatMap(b => allBindingsInProgramIdSchemeExp(b._2))).appendedAll(let.body.flatMap(allBindingsInProgramIdSchemeExp))
      case _ => expr.subexpressions.flatMap(e => allBindingsInProgramIdSchemeExp(e))

  def allBindingsInprogram(expr: Expression): Map[Expression, Identifier] =
    allBindingsInProgramIdSchemeExp(expr).map(_.swap).toMap

  def findLexicalScopes(expr: Expression, currentScope: Map[String, Identifier] = Map()): BindingsWithScopes =
    var newScope = currentScope
    var toReturn: BindingsWithScopes = Map()
    expr match
      case let: SchemeLet =>
        let.bindings.foreach(b =>
          newScope = newScope + (b._1.name -> b._1)
          toReturn = toReturn ++ Map(b._2 -> (b._1, currentScope.filter(variable => b._2.fv.contains(variable._1)))) ++ findLexicalScopes(b._2, currentScope))
        toReturn ++ let.body.flatMap(exp => findLexicalScopes(exp, newScope))
      case let: SchemeLetrec =>
        let.bindings.foreach(b =>
          newScope = newScope + (b._1.name -> b._1))
        let.bindings.foreach(b =>
          toReturn = toReturn ++ Map(b._2 -> (b._1, newScope.filter(variable => b._2.fv.contains(variable._1)))) ++ findLexicalScopes(b._2, newScope))
        toReturn ++ let.body.flatMap(exp => findLexicalScopes(exp, newScope))
      case let: SchemeLetStar =>
        let.bindings.foreach(b =>
          toReturn = toReturn ++ Map(b._2 -> (b._1, newScope.filter(variable => b._2.fv.contains(variable._1)))) ++ findLexicalScopes(b._2, newScope)
          newScope = newScope + (b._1.name -> b._1))
        toReturn ++ let.body.flatMap(exp => findLexicalScopes(exp, newScope))
      case _ =>
        if expr.height > 1 then
          expr.subexpressions.flatMap(findLexicalScopes(_, newScope)).toMap
        else
          Map()


  @tailrec
  final def findLatestInScope(toFind: Map[String, Option[Identifier]], expr: Expression, program: List[(Identifier, Expression)]): Map[String, Option[Identifier]] = program match
    case List() =>
      toFind
    case (id, binding) :: rest if toFind.contains(id.name) =>
        findLatestInScope(toFind + (id.name -> Some(id)), expr, rest)
    case (id, binding) :: rest if up.findAllSubExps(binding).contains(expr) =>
      val newBindings = up.findAllSubExps(binding).collect {
        // In case of let: let must not contain the expression itself (other expressions of let will be out of scope) and the beginning line of let must not be larger than the expression line (out of scope)
        case let: SchemeLet if let.idn.idn.line <= expr.idn.idn.line && !let.bindings.exists(b => b._2.eql(expr)) =>
          let.bindings
        // Let*: bindings know of each other and can call eachother, but not ones that are defined later
        case letstar: SchemeLetStar if letstar.idn.idn.line <= expr.idn.idn.line =>
          letstar.bindings.filter(e => e._2.idn.idn.line < expr.idn.idn.line)
        // Letrec: bindings can call each other even if they are defined later within the same letrec bindings
        case letrec: SchemeLetrec if letrec.idn.idn.line <= expr.idn.idn.line  =>
          letrec.bindings
      }.flatten
      findLatestInScope(toFind, expr, rest.appendedAll(newBindings))
    case (id, binding) :: rest =>
      findLatestInScope(toFind, expr, rest)





case class differentChanges(reanalyse: ReanalysisList, renamings: RenamingsList, ifs: IfsList, scopeChanges: ScopeChanges)