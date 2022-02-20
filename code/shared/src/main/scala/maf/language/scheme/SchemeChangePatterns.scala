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
      var bindingsVars = bindings.flatMap(binding => findAllVarsInOrder(binding._2))
      var bodyVars = body.flatMap(e => findAllVarsInOrder(e))
      bindings.map(binding => binding._1).appendedAll(bindingsVars).appendedAll(bodyVars)
    case SchemeLetStar(bindings, body, pos) =>
      var bindingsVars = bindings.flatMap(binding => findAllVarsInOrder(binding._2))
      var bodyVars = body.flatMap(e => findAllVarsInOrder(e))
      bindings.map(binding => binding._1).appendedAll(bindingsVars).appendedAll(bodyVars)
    case SchemeLetrec(bindings, body, pos) =>
      var bindingsVars = bindings.flatMap(binding => findAllVarsInOrder(binding._2))
      var bodyVars = body.flatMap(e => findAllVarsInOrder(e))
      bindings.map(binding => binding._1).appendedAll(bindingsVars).appendedAll(bodyVars)
    // In case of a list of expressions, look at each expression individually
    case exps: List[_] =>
      exps.flatMap(e => findAllVarsInOrder(exps))
    // In case of a single expression, look at the subexpressions (if there are any)
    case e: Expression =>
      if !e.subexpressions.isEmpty then
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

  def compareRenamingsBindings(oldname: Identifier, newname: Identifier, oldexp: SchemeExp, nwexp: SchemeExp): Boolean =
    val mappedVars = Map(newname.name -> oldname.name)
    oldexp.eql(SchemeChangeRenamerForPatterns.rename(nwexp, mappedVars, Map[String, Int]())._1)

  def findLowestChangedSubExpressions(old: Expression, nw: Expression): List[((Option[Expression], Option[Expression]), Option[Map[Identifier, Identifier]])] =
    if old.eql(nw) then
      return List()
    if old.subexpressions.isEmpty && nw.subexpressions.isEmpty then
      return List(((Some(old), Some(nw)), None))
    val differentOlds = old.subexpressions.partition(oe => nw.subexpressions.exists(ne => oe.idn == ne.idn))
    val differentNews = nw.subexpressions.partition(ne => old.subexpressions.exists(oe => oe.idn == ne.idn))
    val updated: List[((Option[Expression], Option[Expression]), Option[Map[Identifier, Identifier]])] =
      old.subexpressions.filter(oe => nw.subexpressions.exists(ne => oe.idn == ne.idn && !oe.eql(ne)))
        .map(oe => nw.subexpressions.find(ne => oe.idn == ne.idn) match
          case Some(ne) =>
            val renamings = checkRenamingsVariables(oe, ne)
            if renamings._1 then
              ((Some(oe), Some(ne)), Some(renamings._2))
            else ((Some(oe), Some(ne)), None))
    val deletedExps = differentOlds._2.map(e => ((Some(e), None), None))
    val insertedExps = differentNews._2.map(e => ((None, Some(e)), None))
    if (deletedExps.nonEmpty || insertedExps.nonEmpty) || (!old.subexpressions.exists(oe => nw.subexpressions.exists(ne => oe.eql(ne))) || !nw.subexpressions.exists(ne => old.subexpressions.exists(oe => oe.eql(ne)))) then
      return updated.appendedAll(deletedExps).appendedAll(insertedExps)
    old.subexpressions.flatMap(oe =>
      nw.subexpressions.find(ne => oe.idn == ne.idn) match
        case Some(x) => findLowestChangedSubExpressions(oe, x)
        case None => List())


  // Returns a list of expressions that needs to be reanalysed (old and new), and a list of tuples of expressions that are just renamings together with their mappings
  def comparePrograms(old: SchemeExp, nw: SchemeExp): Unit = // (List[maf.core.Expression], List[((maf.core.Expression, maf.core.Expression), (Boolean, Map[Identifier, Identifier]))]) =
    var reanalyse: List[(Option[maf.core.Expression], Option[maf.core.Expression])] = List()
    var rename: List[((maf.core.Expression, maf.core.Expression), (Boolean, Map[Identifier, Identifier]))] = List()
    (old, nw) match
      case (oldlet: SchemeLettishExp, newlet: SchemeLettishExp) =>
        oldlet.bindings.foreach(oe =>
          if !newlet.bindings.exists(ne => oe._1.idn == ne._1.idn) then
            reanalyse = reanalyse.::((Some(oe._2), None)))
        newlet.bindings.foreach(ne =>
          if !oldlet.bindings.exists(oe => oe._1.idn == ne._1.idn) then
            reanalyse = reanalyse.::((None, Some(ne._2))))
        println("to reanalyze: ")
        println(reanalyse)
        val changedBindings  = oldlet.bindings.filter(oe =>
          newlet.bindings.exists(ne => (oe != ne) && (oe._1.idn == ne._1.idn)))
        val changedBindingsOldNew = changedBindings.map(oe =>
          newlet.bindings.find(ne => (oe != ne) && (oe._1.idn == ne._1.idn)) match
            case Some(x) => (oe, x)
          )
        val renamedBindings = changedBindingsOldNew.filter(e => compareRenamingsBindings(e._1._1, e._2._1, e._1._2, e._2._2))
        println("renamed: ")
        println(renamedBindings)
        println("changed:")
        println(changedBindingsOldNew.filterNot(e => renamedBindings.contains(e)))
        val changedBindingsBoth = oldlet.bindings.collect {
          case oe if newlet.bindings.exists(ne => oe._1.idn == ne._1.idn) =>
            newlet.bindings.find(ne => oe._1.idn == ne._1.idn) match
              case Some(x) => (oe, x)
        }
        changedBindingsBoth.filterNot(e => renamedBindings.contains(e)).foreach(e =>
          println("Expressions: " + e._1._2.toString + " " + e._2._2.toString)
          println("lowest changed subexpression: " + findLowestChangedSubExpressions(e._1._2, e._2._2)))



