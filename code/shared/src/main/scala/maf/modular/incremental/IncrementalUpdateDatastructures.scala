package maf.modular.incremental

//import maf.cli.runnables
import maf.core.*
import maf.language.scheme.interpreter.BaseSchemeInterpreter
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo.VarAddr
import maf.language.scheme.interpreter.ConcreteValues.Value.Clo
import maf.language.scheme.interpreter.ConcreteValues.{Addr, AddrInfo}
import maf.language.scheme.lattices.{ModularSchemeLattice, SchemeOp}
import maf.language.scheme.*
import maf.lattice.interfaces.*
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice.incrementalSchemeLattice
import maf.modular.incremental.scheme.lattice.{IncrementalLattice, IncrementalModularSchemeLattice, IncrementalSchemeLattice, IncrementalSchemeTypeDomain}
import maf.modular.scheme.modf.{NoContext, SchemeModFComponent}
import maf.modular.scheme.{ModularSchemeLatticeWrapper, SchemeAddr}


class IncrementalUpdateDatastructures {

  var changedVars: Map[maf.core.Identifier, maf.core.Identifier] = Map()
  var changedExpressions: Map[maf.core.Expression, maf.core.Expression] = Map()
  var allExpressionsInChange: Map[maf.core.Expression, maf.core.Expression] = Map()

  type VarAddr = maf.modular.scheme.VarAddr[_]
  type RetAddr = maf.modular.ReturnAddr[SchemeModFComponent]
  type PtrAddr = maf.modular.scheme.PtrAddr[_]
  type Value = maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice.Value

  def changeDataStructures(a: IncrementalModAnalysis[SchemeExp], exp: SchemeExp): Boolean =
    val changed = SchemeChangePatterns.checkForRenamingParameter(exp)
    val changedVarsSwapped = changed.flatMap(e => e._2).toMap
    changedVars = changedVarsSwapped.map(_.swap).toMap
    changedExpressions = changed.map(e => e._1).toMap

    val allOldExps = changedExpressions.flatMap(e => findAllSubExps(e._1))
    val allNewExps = changedExpressions.flatMap(e => findAllSubExps(e._2))
    allExpressionsInChange = allOldExps.zip(allNewExps).toMap

    println(allExpressionsInChange)

    a match
      case analysis: IncrementalGlobalStore[SchemeExp] =>
        updateStore(analysis)
    true

  // Function to find all the lambda expressions within an expression (needed later because inner lambdas also have changes in terms of position and more when outerlambdas change)
  def findAllLambdas(expr: Expression): List[Expression] =
    expr match
      case _: SchemeLambdaExp => List(expr).appendedAll(expr.subexpressions.flatMap(e => findAllLambdas(e)))
      case _ =>
        if expr.subexpressions.isEmpty then
          List()
        else expr.subexpressions.flatMap(e => findAllLambdas(e))

  def findAllSubExps(expr: Expression): List[Expression] =
    if expr.subexpressions.isEmpty then
      List()
    else List(expr).appendedAll(expr.subexpressions.flatMap(e => findAllSubExps(e)))

  // Update the store in case there is one
  // There are two types of changes: changes to keys that have a variable address and changes to keys that have a return address
  def updateStore(a: IncrementalGlobalStore[SchemeExp]): Unit =
    a.store.foreach((k, v) =>
      (k, v) match
        case (k: VarAddr, _) =>
          updateVarAddr(a, k, v)
        case (k: RetAddr, _) =>
          updateReturnAddr(a, k, v)
        case (k: PtrAddr, _) =>
          updatePointerAddr(a, k, v)
        case _ =>
    )

  // In case we need to update the address: there are two options.
  // If the address is of a variable that is directly affected (like a becoming b), we simply remove that from the store
  // We then insert a new key (with the new variable). The value might change or it may not (if not, getNewValue will just return the old value)
  // If the address is not of a directly affected variable (such as a varAddr of a function), the value might still change and we might still have to update it
  def updateVarAddr(a: IncrementalGlobalStore[SchemeExp], key: VarAddr, value: a.Value): Unit =
    val newValue = getNewValues(a, key, value)
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
  def updateReturnAddr(a: IncrementalGlobalStore[SchemeExp], key: RetAddr, value: a.Value): Unit =
    val newValue = getNewValues(a, key, value)
    key.cmp match
      case SchemeModFComponent.Main =>
        if newValue != value then
          a.store = a.store + (key -> newValue)
      case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), ctx: _) =>
        val changeToLambda = allExpressionsInChange.get(lam)
        changeToLambda match
          case Some(lambda: SchemeLambdaExp) =>
            var newEnv = createNewEnvironment(env)
            val newCmp = SchemeModFComponent.Call(clo = (lambda, new BasicEnvironment[Address](newEnv)), ctx = ctx)
            val newIdn = lambda.subexpressions.last.idn
            val newKey = key.copy(idn = newIdn, cmp = newCmp)
            a.store = a.store - key
            a.store = a.store + (newKey -> newValue)
          case _ =>
            if newValue != value then
              a.store = a.store + (key -> newValue)
      case _ =>

  def updatePointerAddr(a: IncrementalGlobalStore[SchemeExp], key: PtrAddr, value: a.Value): Unit =
    val newValue = getNewValues(a, key, value)
    val newKey = getNewPointerAddr(key)
    a.store = a.store + (newKey -> newValue)
    if allExpressionsInChange contains key.exp then
      a.store = a.store - key

  def getNewPointerAddr(addr: PtrAddr): PtrAddr =
    val changeToExp = allExpressionsInChange.get(addr.exp)
    changeToExp match
      case Some(exp: SchemeExp) =>
        addr.copy(exp = exp)
      case _ =>
        addr

  // The value (maybe) only needs to change if the value before was a Closure (things such as Int do not change as it shouldn't change with simple refactorings)
  // We first set the newExpr to the old value (as it might not change after all) and then we go over all the closures of the value
  // If that closure contains a lambda (e._1),  then we look if that lambda existed (within) a changed lambda, because only in that case the value should change
  // If it is in a changed expression, we get the lambda that is was changed into and create a new closure from that. This will then become newExpr and thereby the new value that is inserted in the store
  // Otherwise the newExpr will just stay the value
  def getNewValues(a: IncrementalGlobalStore[SchemeExp], key: Address, value: a.Value): a.Value =
    value match
      case element: IncrementalSchemeTypeDomain.modularLattice.AnnotatedElements =>
        var newValues = element.values
        newValues= element.values.map(e => getNewValue(a, key, e))
        IncrementalSchemeTypeDomain.modularLattice.AnnotatedElements(newValues, element.sources).asInstanceOf[a.Value]
      case element: IncrementalSchemeTypeDomain.modularLattice.Elements =>
        val newElems = element.vs.map(e => getNewValue(a, key, e))
        IncrementalSchemeTypeDomain.modularLattice.Elements(newElems).asInstanceOf[a.Value]
      case _ => value

  def getNewValue(a: IncrementalGlobalStore[SchemeExp], key: Address, value:  Value): Value =
    value match
      case clos : IncrementalSchemeTypeDomain.modularLattice.Clo =>
        var newClos: Set[maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice.schemeLattice.Closure] = clos.closures.map(e =>
          allExpressionsInChange.get(e._1) match
            case Some(lambda: SchemeLambdaExp) =>
              e._2 match
                case env : maf.core.BasicEnvironment[_] =>
                  var newEnv = createNewEnvironment(env)
                  (lambda, new BasicEnvironment[Address](newEnv))
                case _ => e
            case _ => e)
        IncrementalSchemeTypeDomain.modularLattice.Clo(newClos)
      case vector: IncrementalSchemeTypeDomain.modularLattice.Vec =>
        var isFalse = false
        val newElementsVector = vector.elements.map((k, vecelem) =>
          val nw = getNewValues(a, key, vecelem.asInstanceOf[a.Value])
          (k, nw))
        val newVector = IncrementalSchemeTypeDomain.modularLattice.Vec(size = vector.size, elements = newElementsVector.asInstanceOf[vector.elements.type])
        newVector
      case pointer: IncrementalSchemeTypeDomain.modularLattice.Pointer =>
        IncrementalSchemeTypeDomain.modularLattice.Pointer(pointer.ptrs.map(p => p match
          case pa: PtrAddr =>
            getNewPointerAddr(pa)
          case a: _ =>
            println(a)
            a))
      case cons: IncrementalSchemeTypeDomain.modularLattice.Cons =>
        (cons.car, cons.cdr) match
          case (car: a.Value, cdr: a.Value) =>
            val newcar = getNewValues(a, key, car).asInstanceOf[cons.car.type]
            val newcdr = getNewValues(a, key, cdr).asInstanceOf[cons.cdr.type]
            IncrementalSchemeTypeDomain.modularLattice.Cons(newcar, newcdr)
      case _ =>
        value


  def createNewEnvironment(oldEnv: maf.core.BasicEnvironment[_]): Map[String, Address] =
    var newEnv: Map[String, Address] = Map()
    oldEnv.content.foreach((k, v) =>
      v match
        case varAddr: VarAddr =>
          var oldIdn = varAddr.idn
          (changedVars.find((k , v) => k.idn == oldIdn)) match
            case Some(identifiers) =>
              newEnv += (identifiers._2.name -> varAddr.copy(id = identifiers._2))
            case _ => newEnv += (k -> v)
        case _ => newEnv += (k -> v))
    newEnv
}
