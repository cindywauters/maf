package maf.language.scheme

import maf.core.*
import maf.core.Label
import maf.language.change.ChangeExp
import maf.modular.incremental.IncrementalModAnalysis
import maf.core.Expression

import scala.::

object SchemeChangePatterns:

// find all the changed expressions and create a set with all the old and new expressions
  def findAllChangedExpressions(expr: Expression): Set[(Expression, Expression)] = expr match
    case e: ChangeExp[Expression] => Set((e.old, e.nw)) // Assumption: change expressions are not nested.
    case e                  => e.subexpressions.asInstanceOf[List[Expression]].flatMap(findAllChangedExpressions).toSet


// Get a list of all the variables that are declared within an expression (through lambdas, let, letrec, or let*)
  def findAllVarsInOrder(exp: Expression): List[Identifier] = exp match
    // In case of lambda, add the arguments and look at the body for more
    case SchemeLambda(name, args, body, pos) =>
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
        // renaming turned off for now, turn on earlier in the process?
        // only turned off to test some changes in datastructures
        // will cause problems with some things, check out later
        var renamedOld = oe //SchemeChangeRenamer.rename(oe)
        var renamedNew = ne //SchemeChangeRenamer.rename(ne)
        var variablesOld = findAllVarsInOrder(renamedOld)
        var variablesNew = findAllVarsInOrder(renamedNew)
        if variablesOld.length != variablesNew.length then
          return (false, Map())
        var mappedVars = variablesNew.map(e => e.name).zip(variablesOld.map(e => e.name)).toMap
        var mappedIdentifiers = variablesNew.zip(variablesOld).toMap
       // println(renamedOld)
       // println(SchemeChangeRenamer.rename(renamedNew, mappedVars, Map[String, Int]())._1)
        if renamedOld.eql(SchemeChangeRenamer.rename(renamedNew, mappedVars, Map[String, Int]())._1) then
          //mappedIdentifiers.foreach((e1 , e2) => println(e1._1 + " " + e1._2 + " " + e2._1 + " " + e2._2))
          return (true, mappedIdentifiers.filter(vr => vr._1 != vr._2))
    (false, Map())

  def checkForRenamingParameter(exp: SchemeExp): Set[((maf.core.Expression, maf.core.Expression), Map[maf.core.Identifier, maf.core.Identifier])] =
    var changedExpr = findAllChangedExpressions(exp)
    val renamings: Set[(Boolean, Map[Identifier, Identifier])] = changedExpr.map(e => checkRenamingsVariables(e._1, e._2))
    changedExpr.zip(renamings.map(e => e._2))
    //changedExpr.foreach(e => println("\n" + e._1.toString + "\n" + e._2.toString + "\n is consistant renaming: " + checkRenamingsVariables(e._1, e._2)._1.toString))
    
