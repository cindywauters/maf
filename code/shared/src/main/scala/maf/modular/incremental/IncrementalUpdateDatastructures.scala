package maf.modular.incremental

//import maf.cli.runnables
import maf.aam.scheme.SchemeStoreAllocateReturn
import maf.core.*
import maf.language.scheme.interpreter.BaseSchemeInterpreter
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo.VarAddr
import maf.language.scheme.interpreter.ConcreteValues.Value.Clo
import maf.language.scheme.interpreter.ConcreteValues.{Addr, AddrInfo}
import maf.language.scheme.lattices.{ModularSchemeLattice, SchemeOp}
import maf.language.scheme.*
import maf.lattice.interfaces.*
import maf.modular.AddrDependency
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice.incrementalSchemeLattice
import maf.modular.incremental.scheme.lattice.{IncrementalLattice, IncrementalModularSchemeLattice, IncrementalSchemeLattice, IncrementalSchemeTypeDomain}
import maf.modular.scheme.modf.{NoContext, SchemeModFComponent}
import maf.modular.scheme.{ModularSchemeLatticeWrapper, PrmAddr, SchemeAddr}


class IncrementalUpdateDatastructures {

  type VarAddr = maf.modular.scheme.VarAddr[_]
  type RetAddr = maf.modular.ReturnAddr[SchemeModFComponent]
  type PtrAddr = maf.modular.scheme.PtrAddr[_]
  type PrmAddr = maf.modular.scheme.PrmAddr
  type Value = maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice.Value
  type Depencency = maf.modular.Dependency

  var changedVars: Map[maf.core.Identifier, maf.core.Identifier] = Map()
  var changedExpressions: Map[maf.core.Expression, maf.core.Expression] = Map()
  var allExpressionsInChange: Map[maf.core.Expression, maf.core.Expression] = Map()

  // Call this function when you want to update all the datastructures of an analysis
  // Arguments are an analysis and the expression that is being analysed
  def changeDataStructures(a: IncrementalModAnalysis[SchemeExp], exp: SchemeExp): Boolean =
    val changed = SchemeChangePatterns.checkForRenamingParameter(exp) // get all renamings
    val changedVarsSwapped = changed.flatMap(e => e._2).toMap
    changedVars = changedVarsSwapped.map(_.swap).toMap // Get all renamed vars
    changedExpressions = changed.map(e => e._1).toMap // Get all expressions that have been changed

    // get all expressions that exist within an old expression and in a new expression and zip them together to know what has changed to what
    val allOldExps = changedExpressions.flatMap(e => findAllSubExps(e._1))
    val allNewExps = changedExpressions.flatMap(e => findAllSubExps(e._2))
    allExpressionsInChange = allOldExps.zip(allNewExps).toMap

    a match
      case analysis: IncrementalGlobalStore[SchemeExp] => // Update the store
        updateStore(analysis)
      case _ =>

    updateDependencies(a)
    true

  // Find all the subexpressions of an expression, and their subexpressions.
  // Something like (lambda (a) (+ a 1)) will become List((lambda (a) (+ a 1)), (+ a 1), +, a, 1)
  private def findAllSubExps(expr: Expression): List[Expression] =
    if expr.subexpressions.isEmpty && expr.height == 1 then
      List(expr)
    else if  expr.subexpressions.isEmpty then
      List()
    else List(expr).appendedAll(expr.subexpressions.flatMap(e => findAllSubExps(e)))

  // Update the store in case there is one
  // There are three types of keys that can contain changes: Variable addresses, Return addresses, and Pointer Addresses
  // Primitive address can not change
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

  def insertInStore(a: IncrementalGlobalStore[SchemeExp], oldKey: Address, newKey: Address, value: a.Value): Unit =
    if newKey.equals(oldKey) then
      a.store = a.store + (oldKey -> value)
    else
      a.store = a.store - oldKey
      a.store = a.store + (newKey -> value)

  def insertInDeps(a: IncrementalModAnalysis[SchemeExp], oldKey: maf.modular.Dependency, newKey: maf.modular.Dependency, value: Set[a.Component]): Unit =
    if newKey.equals(oldKey) then
      a.deps = a.deps + (oldKey -> value)
    else
      a.deps = a.deps - oldKey
      a.deps = a.deps + (newKey -> value)

  // In case we need to update the address there are two options.
  // If the address is of a variable that is directly affected (like a becoming b), we simply remove that a from the store
  // We then insert a new key (with the new variable, in this example b). The value might change or it may not (if not, getNewValue will just return the old value)
  // If the address is not of a directly affected variable (such as a varAddr of a function), the value might still change and we might still have to update it
  def updateVarAddr(a: IncrementalGlobalStore[SchemeExp], key: VarAddr, value: a.Value): Unit =
    val newValue = getNewValues(a, key, value)
    val newKey = getNewVarAddr(key)
    insertInStore(a, key, newKey, newValue)

  def getNewVarAddr(addr: VarAddr): VarAddr =
    if changedVars contains addr.id then
      val newIdn = changedVars.getOrElse(addr.id, addr.id)
      val newAddr = addr.copy(id = newIdn, ctx = addr.ctx)
      newAddr
    else addr

  // Together with the idn of the return address that can change, it contains a component that might need to change, and the value might too
  // First, we look if the key's component is a function call. If it is, we want to find if the lambda of the component exists within a changed expression
  // If it is indeed a lambda that needs changing, allExpressionsInChange.get(lam) will return Some(lambda), otherwise it will return None.
  // In case the lambda needs to change, the enviromnent might too.
  // The new component is created from the new lambda and the new environment, and the new idn becomes the idn of the last subexpression of the new lambda
  // We once again use getNewValue to find the new value, and then remove the old key (and value) from the store, and insert the new key and value
  // The call might also be main, in which case the value might have to change too, but that has no other additional component.
  def updateReturnAddr(a: IncrementalGlobalStore[SchemeExp], key: RetAddr, value: a.Value): Unit =
    val newValue = getNewValues(a, key, value)
    val newKey = getNewRetAddr(key)
    insertInStore(a, key, newKey, newValue)


  def getNewRetAddr(addr: RetAddr): RetAddr =
    addr.cmp match
      case SchemeModFComponent.Main =>
        addr
      case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), ctx: _) =>
        val changeToLambda = allExpressionsInChange.get(lam)
        changeToLambda match
          case Some(lambda: SchemeLambdaExp) =>
            val newCmp = getNewComponent(SchemeModFComponent.Call((lam, env), ctx))
            val newIdn = lambda.subexpressions.last.idn
            val newAddr = maf.modular.ReturnAddr[SchemeModFComponent](idn = newIdn, cmp = newCmp)
            newAddr
          case _ =>
            addr

  def getNewComponent(comp: SchemeModFComponent): SchemeModFComponent =
    comp match
      case SchemeModFComponent.Main =>
        comp
      case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), ctx: _) =>
        val changeToLambda = allExpressionsInChange.get(lam)
        changeToLambda match
          case Some(lambda: SchemeLambdaExp) =>
            var newEnv = createNewEnvironment(env)
            val newCmp = SchemeModFComponent.Call(clo = (lambda, new BasicEnvironment[Address](newEnv)), ctx = ctx)
            newCmp
          case None => comp


  // To update the pointer addresses, we also get the new value, and use a function to calculate the new pointer key.
  // We remove the old key from the store if necessary
  def updatePointerAddr(a: IncrementalGlobalStore[SchemeExp], key: PtrAddr, value: a.Value): Unit =
    val newValue = getNewValues(a, key, value)
    val newKey = getNewPointerAddr(key)
    insertInStore(a, key, newKey, newValue)

  // See if the expression is an expression that exists within a change expression. If not, nothing needs to happen. If so, it now becomes the expression of the new version
  def getNewPointerAddr(addr: PtrAddr): PtrAddr =
    val changeToExp = allExpressionsInChange.get(addr.exp)
    changeToExp match
      case Some(newExp: SchemeExp) =>
        addr.copy(exp = newExp)
      case _ =>
        addr

  // A value can be either annotated elements or elements. In both cases, we want to get all the values within the elements and update each of them
  def getNewValues(a: IncrementalGlobalStore[SchemeExp], key: Address, value: a.Value): a.Value =
    value match
      case element: IncrementalSchemeTypeDomain.modularLattice.AnnotatedElements =>
        var newValues = element.values
        newValues= element.values.map(e => getNewValue(a, key, e))
        IncrementalSchemeTypeDomain.modularLattice.AnnotatedElements(newValues, element.sources).asInstanceOf[a.Value]
      case element: IncrementalSchemeTypeDomain.modularLattice.Elements =>
        val newElems = element.vs.map(e => getNewValue(a, key, e))
        IncrementalSchemeTypeDomain.modularLattice.Elements(newElems).asInstanceOf[a.Value]
     // case _ => value

  // If the value is a set of closures, we want to update both the lambda and enviroment within each closure (if necessary).
  // In case of a vector, we want to loop over each of the elements and update them each accordingly
  // If it is a set of pointers, each of the pointers might need updating. For this, getNewPointerAddr is used
  def getNewValue(a: IncrementalGlobalStore[SchemeExp], key: Address, value:  Value): Value =
    value match
      case clos : IncrementalSchemeTypeDomain.modularLattice.Clo =>
        var newClos: Set[IncrementalSchemeTypeDomain.modularLattice.schemeLattice.Closure] = clos.closures.map(closure =>
          allExpressionsInChange.get(closure._1) match // check if lambda is in a change expression
            case Some(lambda: SchemeLambdaExp) =>
              closure._2 match // update the environment of the lambda if it needs changing
                case env : maf.core.BasicEnvironment[_] =>
                  var newEnv = createNewEnvironment(env)
                  (lambda, new BasicEnvironment[Address](newEnv))
               // case env: _ => (lambda, env)
            case _ => closure // Lambda doesn't exist in a change expression: nothing needs to change
        )
        IncrementalSchemeTypeDomain.modularLattice.Clo(newClos)
      case vector: IncrementalSchemeTypeDomain.modularLattice.Vec =>
        val newElementsVector = vector.elements.map((k, vecelem) =>
          val nw = getNewValues(a, key, vecelem.asInstanceOf[a.Value])
          (k, nw)
        )
        val newVector = IncrementalSchemeTypeDomain.modularLattice.Vec(size = vector.size, elements = newElementsVector.asInstanceOf[vector.elements.type])
        newVector
      case pointer: IncrementalSchemeTypeDomain.modularLattice.Pointer =>
        IncrementalSchemeTypeDomain.modularLattice.Pointer(pointer.ptrs.map(p => p match
          case pa: PtrAddr =>
            getNewPointerAddr(pa)
        //  case a: _ => a
        ))
      case cons: IncrementalSchemeTypeDomain.modularLattice.Cons =>
        (cons.car, cons.cdr) match
          case (car: a.Value, cdr: a.Value) =>
            val newcar = getNewValues(a, key, car).asInstanceOf[cons.car.type]
            val newcdr = getNewValues(a, key, cdr).asInstanceOf[cons.cdr.type]
            IncrementalSchemeTypeDomain.modularLattice.Cons(newcar, newcdr)
      case _ =>
        value

  // To create an new enviroment, loop over the old enviroment
  // If a variable did not change, it can be added to the new environment
  // If it did change, the variable that it changed into needs to be added to the environment
  def createNewEnvironment(oldEnv: maf.core.BasicEnvironment[_]): Map[String, Address] =
    var newEnv: Map[String, Address] = Map()
    oldEnv.content.foreach((k, v) =>
      v match
        case varAddr: VarAddr =>
          var oldIdn = varAddr.idn
          changedVars.find((k , v) => k.idn == oldIdn) match
            case Some(identifiers) =>
              newEnv += (identifiers._2.name -> varAddr.copy(id = identifiers._2))
            case _ => newEnv += (k -> v)
        case _ => newEnv += (k -> v))
    newEnv


  def updateDependencies(a: IncrementalModAnalysis[SchemeExp]): Unit =
    a.deps.foreach((k, v) =>
      k match
        case addrDep: AddrDependency =>
          val newV = v.map(e => e match
            case comp: SchemeModFComponent => getNewComponent(comp).asInstanceOf[a.Component])
          addrDep.addr match
            case k: VarAddr =>
              insertInDeps(a, addrDep, addrDep.copy(addr = getNewVarAddr(k)), newV)
            case k: RetAddr =>
              insertInDeps(a, addrDep, addrDep.copy(addr = getNewRetAddr(k)), newV)
            case k: PtrAddr =>
              insertInDeps(a, addrDep, addrDep.copy(addr = getNewPointerAddr(k)), newV)
            case k: PrmAddr =>
              insertInDeps(a, addrDep, addrDep, newV)
    )
}
