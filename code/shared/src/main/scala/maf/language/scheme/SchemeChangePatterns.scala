package maf.language.scheme

import maf.core._
import maf.core.Label
import maf.language.change.ChangeExp
import maf.modular.incremental.IncrementalModAnalysis
import maf.core.Expression

object SchemeChangePatterns:

  def findAllChangedExpressions(expr: Expression): Set[(Expression, Expression)] = expr match
    case e: ChangeExp[Expression] => Set((e.old, e.nw)) // Assumption: change expressions are not nested.
    case e                  => e.subexpressions.asInstanceOf[List[Expression]].flatMap(findAllChangedExpressions).toSet


  def checkRenamingParameter(oldexpr: Expression, newexpr: Expression): Boolean = (oldexpr, newexpr) match
    case (SchemeLambda(_, oldargs, oldbody, _), SchemeLambda(_, newargs, newbody, _)) => checkRenamingLambdaArg(oldargs, oldbody, newargs, newbody)
    case e => false
  
  def checkRenamingLambdaArg(oldargs: List[Identifier], oldbody: List[SchemeExp], newargs: List[Identifier], newbody: List[SchemeExp]): Boolean =
    var changedArgs: List[(Set[String], Set[String])] = List[(Set[String], Set[String])]()
    var changedExprs: List[(SchemeExp, SchemeExp)] = List[(SchemeExp, SchemeExp)]()
    if oldargs.length == newargs.length && oldbody.length == newbody.length then
       changedArgs = oldargs.zip(newargs).filter((a1, a2) =>  a1.toString() != a2.toString).map((a1, a2) => (a1.fv, a2.fv))
       if changedArgs.isEmpty then
         return false
       changedExprs = oldbody.zip(newbody).filter((b1, b2) => !b1.eql(b2))
       if changedExprs.filter(e1 => checkIndividualExpressions(e1._1, e1._2, changedArgs)).isEmpty then
         return false
       true
    else false


  def checkIndividualExpressions(oldexprs: Expression, newexprs: Expression, changedArgs: List[(Set[String], Set[String])]): Boolean =
    if oldexprs.height > 1 then
       oldexprs.subexpressions.zip(newexprs.subexpressions).forall(e => checkIndividualExpressions(e._1, e._2, changedArgs))
    else
      if (oldexprs.subexpressions.isEmpty || newexprs.subexpressions.isEmpty) && !oldexprs.eql(newexprs) then
        return false
      val differentExpressions = oldexprs.subexpressions.zip(newexprs.subexpressions).filter((e1, e2) => !e1.eql(e2))
      if differentExpressions != List() then
        if differentExpressions.filter((e1, e2) => e1.subexpressions.length == e2.subexpressions.length && changedArgs.contains(e1.fv, e2.fv)) != List() then
         true
        else false
      else true



  def checkForRenamingParameter(exp: SchemeExp): SchemeExp =
    def changedExpr = findAllChangedExpressions(exp)
    changedExpr.foreach(e => println(e.toString() + "\n is consistant renaming: " + checkRenamingParameter(e._1, e._2).toString))
    exp
