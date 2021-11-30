package maf.modular.incremental

//import maf.cli.runnables
import maf.core.*
import maf.language.scheme.interpreter.BaseSchemeInterpreter
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo.VarAddr
import maf.language.scheme.interpreter.ConcreteValues.Value.Clo
import maf.language.scheme.interpreter.ConcreteValues.{Addr, AddrInfo}
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.language.scheme.*
import maf.lattice.interfaces.*
import maf.modular.incremental.scheme.lattice.{IncrementalLattice, IncrementalModularSchemeLattice, IncrementalSchemeLattice}
import maf.modular.scheme.modf.{NoContext, SchemeModFComponent}
import maf.modular.scheme.{ModularSchemeLatticeWrapper, SchemeAddr}


class IncrementalUpdateDatastructures {

  def changeDataStructures(a: IncrementalModAnalysis[SchemeExp], exp: SchemeExp): Boolean =

    var changed = SchemeChangePatterns.checkForRenamingParameter(exp)
    var changedVars = changed.flatMap(e => e._2)
    var ChangedVarsSwapped = changedVars.map(_.swap).toMap
    var changedExpressions = changed.map(e => e._1).toMap

    a match
      case analysis: IncrementalGlobalStore[SchemeExp] =>
        updateStore(analysis, ChangedVarsSwapped, changedExpressions)
    true

  // Function to find all the lambda expressions within an expression (needed later because inner lambdas also have changes in terms of position and more when outerlambdas change)
  def findAllLambdas(expr: Expression): List[Expression] =
    expr match
      case _: SchemeLambdaExp => List(expr).appendedAll(expr.subexpressions.flatMap(e => findAllLambdas(e)))
      case _ =>
        if expr.subexpressions.isEmpty then
          List()
        else expr.subexpressions.flatMap(e => findAllLambdas(e))

  // Update the store in case there is one
  // There are two types of changes: changes to keys that have a variable address and changes to keys that have a return address
  def updateStore(a: IncrementalGlobalStore[SchemeExp], changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): Unit =
    println(a.store)
    a.store.foreach((k, v) =>
      (k, v) match
        case (k: maf.modular.scheme.VarAddr[NoContext.type], _) =>
          updateVarAddr(a, k, v, changedVars, changedExpressions)
        case (k: maf.modular.ReturnAddr[SchemeModFComponent], _) =>
          updateReturnAddr(a, k, v, changedVars, changedExpressions)
        case _ =>
    )
    println(a.store)

  // In case we need to update the address: there are two options.
  // If the address is of a variable that is directly affected (like a becoming b), we simply remove that from the store
  // We then insert a new key (with the new variable). The value might change or it may not (if not, getNewValue will just return the old value)
  // If the address is not of a directly affected variable (such as a varAddr of a function), the value might still change and we might still have to update it
  def updateVarAddr(a: IncrementalGlobalStore[SchemeExp], key: maf.modular.scheme.VarAddr[NoContext.type], value: a.Value, changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): Unit =
    val newValue = getNewValue(a, key, value, changedVars, changedExpressions)
    if changedVars contains key.id then
      val newIdn = changedVars.getOrElse(key.id, key.id)
      val newKey = key.copy(id = newIdn, ctx = key.ctx)
      a.store = a.store - key
      a.store = a.store + (newKey -> newValue)
    else
      a.store = a.store + (key -> newValue)


  // In case of a return address, it is a bit more difficult because the key and its component need to change, and the value might too
  // First, look if the key's component is a function call. If it is, we want to find if the lambda of the component changed (this lambda might be nested in another lambda which is why we need to get all lambdas in the old and new expression and zip them together)
  // If it is indeed a lambda that needs changing, allChangedLambdas.get(lam) will return Some(lambda), otherwise it will return None.
  // We first build up a new environment, by going through the old environment and if it contains a variable that was changed by the expression, we change it accordingly. Unchanged variables are added to the new env too
  // The new component is created from the new lambda and the new environment, and the new idn becomes the idn of the last subexpression of the new lambda
  // We once again use getNewValue to find the new value, and then remove the old key (and value) from the store, and insert the new key and value
  def updateReturnAddr(a: IncrementalGlobalStore[SchemeExp], key: maf.modular.ReturnAddr[SchemeModFComponent], value: a.Value, changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): Unit =
    val newValue = getNewValue(a, key, value, changedVars, changedExpressions)
    key.cmp match
      case SchemeModFComponent.Main =>
        if newValue != value then
          a.store = a.store + (key -> newValue)
      case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), ctx: NoContext.type) =>
        val allOldLambdas = changedExpressions.flatMap(e => findAllLambdas(e._1))
        val allNewLambdas = changedExpressions.flatMap(e => findAllLambdas(e._2))
        val allChangedLambdas = allOldLambdas.zip(allNewLambdas).toMap
        val changeToLambda = allChangedLambdas.get(lam)
        changeToLambda match
          case Some(lambda: SchemeLambdaExp) =>
            var newEnv = createNewEnvironment(env, changedVars)
            val newCmp = SchemeModFComponent.Call(clo = (lambda, new BasicEnvironment[Address](newEnv)), ctx = ctx)
            val newIdn = lambda.subexpressions.last.idn
            val newKey = key.copy(idn = newIdn, cmp = newCmp)
            a.store = a.store - key
            a.store = a.store + (newKey -> newValue)
          case _ =>
            if newValue != value then
              a.store = a.store + (key -> newValue)
      case _ =>

  // The value (maybe) only needs to change if the value before was a Closure (things such as Int do not change as it shouldn't change with simple refactorings)
  // We first set the newExpr to the old value (as it might not change after all) and then we go over all the closures of the value
  // If that closure contains a lambda (e._1),  then we look if that lambda existed (within) a changed lambda, because only in that case the value should change
  // If it is in a changed expression, we get the lambda that is was changed into and create a new closure from that. This will then become newExpr and thereby the new value that is inserted in the store
  // Otherwise the newExpr will just stay the value
  def getNewValue(a: IncrementalGlobalStore[SchemeExp], key: Address, value: a.Value, changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): a.Value =
    a.lattice match
      case l: IncrementalSchemeLattice[_, _]  =>
        var clos = l.getClosures(value)
        var newExpr = value
        clos.map(e =>
          val allOldLambdas = changedExpressions.flatMap(e => findAllLambdas(e._1))
          val allNewLambdas = changedExpressions.flatMap(e => findAllLambdas(e._2))
          val allChangedLambdas = allOldLambdas.zip(allNewLambdas).toMap
          allChangedLambdas.get(e._1) match
            case Some(lambda: SchemeLambdaExp) =>
              e._2 match
                case env : maf.core.BasicEnvironment[_] =>
                  var newEnv = createNewEnvironment(env, changedVars)
                  newExpr = l.closure(lambda, new BasicEnvironment[Address](newEnv))
                case _ =>
            case _ =>
        )
        newExpr
      case _ => value

  def createNewEnvironment(oldEnv: maf.core.BasicEnvironment[_], changedVars: Map[Identifier, Identifier]): Map[String, Address] =
    var newEnv: Map[String, Address] = Map()
    oldEnv.content.foreach((k, v) =>
      v match
        case varAddr: maf.modular.scheme.VarAddr[NoContext.type] =>
          var oldIdn = varAddr.idn
          (changedVars.find((k , v) => k.idn == oldIdn)) match
            case Some(identifiers) =>
              newEnv += (identifiers._2.name -> varAddr.copy(id = identifiers._2))
            case _ => newEnv += (k -> v)
        case _ => newEnv += (k -> v))
    newEnv
}
