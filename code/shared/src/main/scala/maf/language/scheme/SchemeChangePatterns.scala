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


/*
// Check whether both the old and the new expression are lambdas. If so, check for the renaming of the variables
  def checkRenamingParameter(oldexpr: Expression, newexpr: Expression): Boolean = (oldexpr, newexpr) match
    case (SchemeLambda(_, oldargs, oldbody, _), SchemeLambda(_, newargs, newbody, _)) => checkRenamingLambdaArg(oldargs, oldbody, newargs, newbody)
    case e => false
  
  def checkRenamingLambdaArg(oldargs: List[Identifier], oldbody: List[SchemeExp], newargs: List[Identifier], newbody: List[SchemeExp]): Boolean =
    var changedArgs: List[(Set[String], Set[String])] = List[(Set[String], Set[String])]()
    var changedExprs: List[(SchemeExp, SchemeExp)] = List[(SchemeExp, SchemeExp)]()
    // If the arguments' or bodies' length doesn't match up: more than a renaming happened so return false 
    if oldargs.length == newargs.length && oldbody.length == newbody.length then
      // Zip the arguments that have changed together and put them into a list to keep track of what arguments changed names
       changedArgs = oldargs.zip(newargs).filter((a1, a2) =>  a1.toString() != a2.toString).map((a1, a2) => (a1.fv, a2.fv))
       // If no args have changed, return false
       if changedArgs.isEmpty then
         return false
         // Zip the bodies of the old and new expressions together (to compare them). Only compare the expressions that have actually changed
       changedExprs = oldbody.zip(newbody).filter((b1, b2) => !b1.eql(b2))
       // Check per individual expression of the bodies whether the name changes happen in the body
       if changedExprs.filter(e1 => checkIndividualExpressions(e1._1, e1._2, changedArgs)).isEmpty then
         return false
       true
    else false


  def checkIndividualExpressions(oldexprs: Expression, newexprs: Expression, changedArgs: List[(Set[String], Set[String])]): Boolean =
    // Look only at expressions of height 1 (if higher height, look at all the sub expressions)
    if oldexprs.height > 1 then
       oldexprs.subexpressions.zip(newexprs.subexpressions).forall(e => checkIndividualExpressions(e._1, e._2, changedArgs))
    else
      // If the old and new has no subexpressions, and the expressions aren't the same, then return false
      if (oldexprs.subexpressions.isEmpty || newexprs.subexpressions.isEmpty) && !oldexprs.eql(newexprs) then
        return false
      // Zip old and new sub expressions and look where the expressions aren't equal
      val differentExpressions = oldexprs.subexpressions.zip(newexprs.subexpressions).filter((e1, e2) => !e1.eql(e2))
      // If there are indeed different expressions, look if they have the same length, and if the free variables of the old vs new match up with the variables that have changed names
      if differentExpressions != List() then
        if differentExpressions.filter((e1, e2) => e1.subexpressions.length == e2.subexpressions.length && changedArgs.contains(e1.fv, e2.fv)) != List() then
         true
        else false
      else true

*/

// Get a list of all the variables that are declared within an expression (through lambdas, let, letrec, or let*)
  def findAllVarsInOrder(exp: Expression): List[String] = exp match
    // In case of lambda, add the arguments and look at the body for more
    case SchemeLambda(name, args, body, pos) =>
      args.map(arg => arg.name).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
    // In case of let, let* and letrec, take the names of the bindings and look in the body for more
    case SchemeLet(bindings, body, pos) =>
      bindings.map(binding => binding._1.name).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
    case SchemeLetStar(bindings, body, pos) =>
      bindings.map(binding => binding._1.name).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
    case SchemeLetrec(bindings, body, pos) =>
      bindings.map(binding => binding._1.name).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
    // In case of a list of expressions, look at each expression individually
    case exps: List[_] =>
      exps.flatMap(e => findAllVarsInOrder(exps))
    // In case of a single expression, look at the subexpressions (if there are any)
    case e: Expression =>
      if !e.subexpressions.isEmpty then
        e.subexpressions.flatMap(e => findAllVarsInOrder(e))
      else  List()


  def checkRenamingsVariables(oldexp: Expression, newexp: Expression): (Boolean, Map[String, String]) =
    // get the variables of the old and the new expressions
    var variablesOld = findAllVarsInOrder(oldexp)
    var variablesNew = findAllVarsInOrder(newexp)
    // if not the same length -> they can't be the same either way
    if variablesOld.length != variablesNew.length then
      return (false, Map())
    // create a map of the old to the new ones (used for renaming purposes
    var mappedVars = variablesNew.zip(variablesOld).toMap
    newexp match
      case e: SchemeExp =>
        // If the new expression is a scheme expression, rename the variables according to the above mapping
        // If only consisting renaming happened, the old expression should be equal to the renamed new expression
        if oldexp.eql(SchemeChangeRenamer.rename(e, mappedVars, Map[String, Int]())._1) then
        return (true, mappedVars.filter(vr => vr._1 != vr._2))
    (false, Map())

  def checkForRenamingParameter(exp: SchemeExp): SchemeExp =
    def changedExpr = findAllChangedExpressions(exp)
    changedExpr.foreach(e => println("\n" + e._1.toString + "\n" + e._2.toString + "\n is consistant renaming: " + checkRenamingsVariables(e._1, e._2)._1.toString))
    exp
