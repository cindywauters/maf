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

type reanalysisList = List[(Option[Expression], Option[Expression])]
type renamingsList  = List[((Expression, Expression), (Boolean, Map[Identifier, Identifier]))]
type ifsList        = List[((SchemeIf, SchemeIf),  List[Identifier], (Expression, Expression))]

object SchemeChangePatterns:

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

  def findLowestChangedSubExpressions(old: Expression, nw: Expression): reanalysisList =
    if old.eql(nw) then
      return List()
    if old.subexpressions.isEmpty && nw.subexpressions.isEmpty then
      return List((Some(old), Some(nw)))
    (old, nw) match
      case (ol: SchemeLambda, nl: SchemeLambda) =>
        if checkRenamingsVariables(ol, nl)._1 then
          return List((Some(ol), Some(nl)))
      case _ =>
    val differentOlds = old.subexpressions.filterNot(oe => nw.subexpressions.exists(ne => oe.idn == ne.idn))
    val differentNews = nw.subexpressions.filterNot(ne => old.subexpressions.exists(oe => oe.idn == ne.idn))
    val updated = old.subexpressions.filter(oe => nw.subexpressions.exists(ne => oe.idn == ne.idn && !oe.eql(ne)))
      .flatMap(oe =>
        nw.subexpressions.find(ne => oe.idn == ne.idn) match
          case Some(ne) =>
            if oe.subexpressions.exists(oe => ne.subexpressions.exists(ne => oe.idn == ne.idn)) then
              findLowestChangedSubExpressions(oe, ne)
            else
              List((Some(oe), Some(ne))))
    if differentOlds.nonEmpty || differentNews.nonEmpty then
      updated.appendedAll(differentOlds.map(e => (Some(e), None))).appendedAll(differentNews.map(e => (None, Some(e)))).::((Some(old), Some(nw))) // Something is inserted or deleted so we want to return the encapsulating expression as well as the inserted/deleted thing
    else
      updated

  // Returns a list of expressions that needs to be reanalysed (old and new), and a list of tuples of expressions that are just renamings together with their mappings
  def comparePrograms(old: SchemeExp, nw: SchemeExp, analysis: Option[IncrementalModAnalysisWithUpdateTwoVersions[_]] = None): differentChanges =
    var reanalyse: reanalysisList = List()
    var rename: renamingsList = List()
    var needed_prims: List[Identifier] = List()
    var ifs: ifsList = List()
    var inserts: List[Expression] = List()
    var deletes: List[Expression] = List()
    var scopeChanges: List[(Expression, Expression)] = List()
    (old, nw) match
      case (oldlet: SchemeLettishExp, newlet: SchemeLettishExp) =>
        oldlet.bindings.foreach(oe =>
          if !newlet.bindings.exists(ne => oe._2.idn == ne._2.idn) then
            deletes = deletes.::(oe._2)
          if oe._1.idn.idn.tag != Position.noTag then
            needed_prims = needed_prims.::(oe._1))
        newlet.bindings.foreach(ne =>
          if !oldlet.bindings.exists(oe => oe._2.idn == ne._2.idn) then
            inserts = inserts.::(ne._2)
          if ne._1.idn.idn.tag != Position.noTag && !needed_prims.contains(ne._1) then
            needed_prims = needed_prims.::(ne._1))
        val changedBindings  = oldlet.bindings.filter(oe =>
          newlet.bindings.exists(ne => (oe != ne) && (oe._2.idn == ne._2.idn)))
        val changedBindingsOldNew = changedBindings.map(oe =>
          newlet.bindings.find(ne => (oe != ne) && (oe._2.idn == ne._2.idn)) match
            case Some(x) => (oe, x))
        val renamedBindings = changedBindingsOldNew.filter(e => compareRenamingsBindings(e._1._1, e._2._1, e._1._2, e._2._2)._1)
        rename = rename.appendedAll(renamedBindings.map(e => ((e._1._2, e._2._2), (true, compareRenamingsBindings(e._1._1, e._2._1, e._1._2, e._2._2)._2))))
        val changedBindingsBoth = oldlet.bindings.collect {
          case oe if newlet.bindings.exists(ne => oe._2.idn == ne._2.idn) =>
            newlet.bindings.find(ne => oe._2.idn == ne._2.idn) match
              case Some(x) => (oe, x)
        }
        changedBindingsBoth.filterNot(e => renamedBindings.contains(e)).foreach(e =>
          findLowestChangedSubExpressions(e._1._2, e._2._2).foreach(e => e match
            case (Some(oe), None) =>
              deletes = deletes.::(oe)
            case (None, Some(ne)) =>
              inserts = inserts.::(ne)
            case (Some(oe), Some(ne)) => (oe, ne) match
              case (oe: Identifier, ne: Identifier) =>
                if renamedBindings.exists(e => e._1._1.name == oe.name && e._2._1.name == ne.name) then
                  rename = rename.::((oe, ne), (true, Map(ne -> oe)))
              case (oe: Identifier, _) => reanalyse = reanalyse.::((Some(oe), Some(ne)))
              case (_, ne: Identifier) => reanalyse = reanalyse.::((Some(oe), Some(ne)))
              // check for ifs
              case (oldIf: SchemeIf, newIf: SchemeIf) =>
                val maybeSwapped = compareIfs(oldIf, newIf, needed_prims)
                maybeSwapped match
                  case Some((added, conds)) =>
                    ifs = ifs.::((oldIf -> newIf), added, conds)
                  case None =>
                    if oldIf.eql(newIf) then
                      rename = rename.::((oldIf, newIf), (true, Map()))
              case _ =>
                val renamings = checkRenamingsVariables(oe, ne)
                if renamings._1 then
                  rename = rename.::((oe, ne), checkRenamingsVariables(oe, ne))
                else
                  reanalyse = reanalyse.::((Some(oe), Some(ne)))
            case (Some(oe), None)    => reanalyse = reanalyse.::((Some(oe), None))))
        println("218")
        deletes.foreach(deleted =>
          inserts.find(i => i.eql(deleted)) match
            case Some(inserted) =>
              val bindingsin: Map[String, Option[Identifier]] = findLatestInScope(inserted.fv.map(name => (name, None)).toMap, inserted, newlet.bindings)
              var bindingsout: Map[String, Option[Identifier]] = Map()
              analysis match
                case Some(analysis) =>
                  val relatedComponent = analysis.getMapping(deleted)
                  if relatedComponent.size == 1 then
                    println("has an environment")
                    relatedComponent.foreach(comp =>
                      comp match
                        case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                          println(env.content)
                          println(lam)
                          env.content.foreach(e =>
                            if deleted.fv.contains(e._1) then
                              e._2 match
                                case varAddr: maf.modular.scheme.VarAddr[_] =>
                                 bindingsout = bindingsout + (e._1 -> Some(varAddr.id))
                                case prmAddr: maf.modular.scheme.PrmAddr =>
                                 bindingsout = bindingsout + (e._1 -> None)
                                case _ => println(e._2.getClass))
                        case _ =>)
                case None           =>
              if bindingsout.size != bindingsin.size then
                println("has no (matching) environment")
                 bindingsout = findLatestInScope(deleted.fv.map(name => (name, None)).toMap, deleted, oldlet.bindings)
              if bindingsin == bindingsout then
                println("moved scopes: ")
                scopeChanges = scopeChanges.::(deleted, inserted)
              else
                println("different functions: ")
              println(inserted.idn.toString + " " + deleted.idn.toString + " " + inserted.toString)
              bindingsin.foreach(e => e._2 match
                case Some(id) => print(id.idn.toString + " ")
                case _ => print("None "))
              println()
              bindingsout.foreach(e => e._2 match
                case Some(id) => print(id.idn.toString + " ")
                case _ => print("None "))
              println()
            case _ =>)
        allBindingsInprogram(oldlet).foreach(println)
        differentChanges(reanalyse, rename, ifs, scopeChanges)
      case _ => differentChanges(reanalyse, rename, ifs, scopeChanges)

  var up = new IncrementalUpdateDatastructures

  @tailrec
  def findEquivalent(expr: Expression, in: List[Expression]): Option[Expression] = in match
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

  def findLexicalScopes(expr: Expression, currentScope: Map[String, Identifier]): List[Map[Expression, (Identifier, Map[String, Identifier])]] =
    var newScope = currentScope
    var toReturn: List[Map[Expression, (Identifier, Map[String, Identifier])]] = List()
    expr match
      case let: SchemeLet =>
        let.bindings.foreach(b =>
          newScope = newScope + (b._1.name -> b._1)
          toReturn = toReturn.::(Map(b._2 -> (b._1, currentScope.filter(variable => b._2.fv.contains(variable._1))))).appendedAll(findLexicalScopes(b._2, currentScope)))
        toReturn.appendedAll(let.body.flatMap(exp => findLexicalScopes(exp, newScope)))
      case let: SchemeLetrec =>
        let.bindings.foreach(b =>
          newScope = newScope + (b._1.name -> b._1))
        let.bindings.foreach(b =>
          toReturn = toReturn.::(Map(b._2 -> (b._1, newScope.filter(variable => b._2.fv.contains(variable._1))))).appendedAll(findLexicalScopes(b._2, newScope)))
         toReturn.appendedAll(let.body.flatMap(exp => findLexicalScopes(exp, newScope)))
      case let: SchemeLetrec =>
        let.bindings.foreach(b =>
          toReturn = toReturn.::(Map(b._2 -> (b._1, newScope.filter(variable => b._2.fv.contains(variable._1))))).appendedAll(findLexicalScopes(b._2, newScope))
          newScope = newScope + (b._1.name -> b._1))
        toReturn.appendedAll(let.body.flatMap(exp => findLexicalScopes(exp, newScope)))
      case _ => List()



  @tailrec
  def findLatestInScope(toFind: Map[String, Option[Identifier]], expr: Expression, program: List[(Identifier, Expression)]): Map[String, Option[Identifier]] = program match
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





case class differentChanges(reanalyse: reanalysisList, renamings: renamingsList, ifs: ifsList, scopeChanges: List[(Expression, Expression)])