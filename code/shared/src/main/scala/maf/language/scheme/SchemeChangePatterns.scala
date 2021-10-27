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
  def findAllVarsInOrder(
              exp: Expression
            ): List[String] = exp match
    case SchemeLambda(name, args, body, pos) =>
      args.map(arg => arg.name).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
    case SchemeLet(bindings, body, pos) =>
      bindings.map(binding => binding._1.name).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
    case SchemeLetStar(bindings, body, pos) =>
      bindings.map(binding => binding._1.name).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
    case SchemeLetrec(bindings, body, pos) =>
      bindings.map(binding => binding._1.name).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
    case _ => List()



  def checkForRenamingParameter(exp: SchemeExp): SchemeExp =
    def changedExpr = findAllChangedExpressions(exp)
  //  changedExpr.foreach(e => println(e._1.isInstanceOf[SchemeExp]))
  //  changedExpr.foreach(e => rename(e._1))
    changedExpr.foreach(e => println(findAllVarsInOrder(e._1)))
    changedExpr.foreach(e => println("\n" + e._1.toString + "\n" + e._2.toString + "\n is consistant renaming: " + checkRenamingParameter(e._1, e._2).toString))
    exp
