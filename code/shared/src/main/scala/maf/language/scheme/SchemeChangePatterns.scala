package maf.language.scheme

import maf.core.*
import maf.core.Label
import maf.language.change.ChangeExp
import maf.modular.incremental.IncrementalModAnalysis
import maf.core.Expression

import scala.::

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

  def checkForRenamingParameter(exp: SchemeExp): Set[((maf.core.Expression, maf.core.Expression), (Boolean, Map[Identifier, Identifier]))] =
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
                Some(prims.filter(e => e.name == newId.name), (oldFun, newFun))
              else None
            else None
          case (_, _) =>
            None
      case (oldVal: SchemeValue, newFun: SchemeFuncall) =>
        newFun.f match
          case SchemeVar(newId) =>
            if newId.name == "not" then
              if comparePartialCondAndBranches(List(oldVal), newFun.args, oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                Some(prims.filter(e => e.name == "not"), (oldVal, newFun.args.head))
              else None
            else None
          case _ => None
      case (oldFun: SchemeFuncall, newVal: SchemeValue) =>
        oldFun.f match
          case SchemeVar(oldId) =>
            if oldId.name == "not" then
              if comparePartialCondAndBranches(oldFun.args, List(newVal), oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                Some(List(), (oldFun.args.head, newVal))
              else None
            else None
          case _ => None
      case (e1: _, e2: _) => None

  def findLowestChangedSubExpressions(old: Expression, nw: Expression): List[(Option[Expression], Option[Expression])] =
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
      updated.::((Some(old), Some(nw))) // Something is inserted or deleted so we want to return the encapsulating expression
    else
      updated

  // Returns a list of expressions that needs to be reanalysed (old and new), and a list of tuples of expressions that are just renamings together with their mappings
  def comparePrograms(old: SchemeExp, nw: SchemeExp): differentChanges =
    var reanalyse: List[(Option[maf.core.Expression], Option[maf.core.Expression])] = List()
    var rename: List[((maf.core.Expression, maf.core.Expression), (Boolean, Map[Identifier, Identifier]))] = List()
    var needed_prims: List[Identifier] = List()
    var ifs: List[((SchemeIf, SchemeIf),  List[Identifier], (Expression, Expression))] = List()
    (old, nw) match
      case (oldlet: SchemeLettishExp, newlet: SchemeLettishExp) =>
        oldlet.bindings.foreach(oe =>
          if !newlet.bindings.exists(ne => oe._1.idn == ne._1.idn) then
            reanalyse = reanalyse.::((Some(oe._2), None))
          if oe._1.idn.idn.tag != Position.noTag then
            needed_prims = needed_prims.::(oe._1))
        newlet.bindings.foreach(ne =>
          if !oldlet.bindings.exists(oe => oe._1.idn == ne._1.idn) then
            reanalyse = reanalyse.::((None, Some(ne._2)))
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
            case (Some(oe), None)    => reanalyse = reanalyse.::((Some(oe), None))
          )
          )
        differentChanges(reanalyse, rename, ifs)
      case _ => differentChanges(reanalyse, rename, ifs)




case class differentChanges(reanalyse: List[(Option[maf.core.Expression], Option[maf.core.Expression])], renamings: List[((maf.core.Expression, maf.core.Expression), (Boolean, Map[Identifier, Identifier]))], ifs: List[((SchemeIf, SchemeIf),  List[Identifier], (Expression, Expression))])