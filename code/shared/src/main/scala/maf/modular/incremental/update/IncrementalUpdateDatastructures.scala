package maf.modular.incremental.update

//import maf.cli.runnables
import maf.aam.scheme.SchemeStoreAllocateReturn
import maf.core.*
import maf.language.scheme.*
import maf.language.scheme.interpreter.BaseSchemeInterpreter
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo.VarAddr
import maf.language.scheme.interpreter.ConcreteValues.Value.Clo
import maf.language.scheme.interpreter.ConcreteValues.{Addr, AddrInfo}
import maf.language.scheme.lattices.{ModularSchemeLattice, SchemeOp}
import maf.lattice.interfaces.*
import maf.modular.AddrDependency
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice.incrementalSchemeLattice
import maf.modular.incremental.scheme.lattice.*
import maf.modular.incremental.{IncrementalGlobalStore, IncrementalModAnalysis}
import maf.modular.scheme.modf.{NoContext, SchemeModFComponent}
import maf.modular.scheme.{ModularSchemeLatticeWrapper, PrmAddr, SchemeAddr}


class IncrementalUpdateDatastructures {

  type VarAddr = maf.modular.scheme.VarAddr[_]
  type RetAddr = maf.modular.ReturnAddr[_]
  type PtrAddr = maf.modular.scheme.PtrAddr[_]
  type PrmAddr = maf.modular.scheme.PrmAddr
  type Value = maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice.Value

  var changedVars: Map[maf.core.Identifier, maf.core.Identifier] = Map()
  var changedExpressions: Map[maf.core.Expression, maf.core.Expression] = Map()
  var allExpressionsInChange: Map[maf.core.Expression, maf.core.Expression] = Map()
  var allIfs: List[((SchemeIf, SchemeIf),  List[Identifier], (Expression, Expression))] = List()

  // Call this function when you want to update all the datastructures of an analysis
  // Arguments are an analysis and the expression that is being analysed
  def changeDataStructures(a: IncrementalModAnalysis[Expression], exp: Expression, renamings: Set[((maf.core.Expression, maf.core.Expression), Map[maf.core.Identifier, maf.core.Identifier])], ifs: List[((SchemeIf, SchemeIf),  List[Identifier], (Expression, Expression))] = List()): Boolean =

    val changedVarsSwapped = renamings.flatMap(e => e._2).toMap
    changedVars = changedVarsSwapped.map(_.swap).toMap // Get all renamed vars
    changedExpressions = renamings.map(e => e._1).toMap // Get all expressions that have been changed

    // get all expressions that exist within an old expression and in a new expression and zip them together to know what has changed to what
    val allOldExps = changedExpressions.flatMap(e => findAllSubExps(e._1))
    val allNewExps = changedExpressions.flatMap(e => findAllSubExps(e._2))
    allExpressionsInChange = allOldExps.zip(allNewExps).toMap
    ifs.foreach(e =>
      val oldIf = e._1._1
      val oldIfCondSubs = findAllSubExps(oldIf.cond)
      val newIf = e._1._2
      val newIfCondSubs = findAllSubExps(newIf.cond)
      findAllSubExps(e._3._1).zip(findAllSubExps(e._3._2)).foreach(e => allExpressionsInChange = allExpressionsInChange + (e._1 -> e._2))
      findAllSubExps(oldIf.cons).zip(findAllSubExps(newIf.alt)).foreach(e => allExpressionsInChange = allExpressionsInChange + (e._1 -> e._2))
      findAllSubExps(oldIf.alt).zip(findAllSubExps(newIf.cons)).foreach(e => allExpressionsInChange = allExpressionsInChange + (e._1 -> e._2))
      allExpressionsInChange = allExpressionsInChange + (oldIf -> newIf)

    )
    allIfs = ifs
    ifs.foreach(e =>
      if e._2.exists(e => e.name == "not") then
        println("60")
        println(e._3._1)
        println(e._3._2)
        a.mapping.get(e._3._1) match
          case Some(mapping) =>
            e._1._2.cond match
              case cond: SchemeFuncall =>
                a.mapping += (cond -> mapping)
                a.mapping += (cond.f -> mapping)
    )


    a match
      case analysis: IncrementalGlobalStore[Expression] => // Update the store
        updateStore(analysis)
      case _ =>
    updateDependencies(a) // Update the dependencies
    updateMapping(a) // Update the store
    updateVisited(a) // Update visited
    true

  // Find all the subexpressions of an expression, and their subexpressions.
  // Something like (lambda (a) (+ a 1)) will become List((lambda (a) (+ a 1)), (+ a 1), +, a, 1)
  def findAllSubExps(expr: Expression): List[Expression] =
    if expr.subexpressions.isEmpty && expr.height == 1 then
      List(expr)
    else if  expr.subexpressions.isEmpty then
      List()
    else expr match
      case lambda: SchemeLambda => List(expr).appended(SchemeBegin(lambda.body, lambda.body.head.idn)).appendedAll(expr.subexpressions.flatMap(e => findAllSubExps(e)))
      case _ => List(expr).appendedAll(expr.subexpressions.flatMap(e => findAllSubExps(e)))

  // Update the store in case there is one
  // There are three types of keys that can contain changes: Variable addresses, Return addresses, and Pointer Addresses. Primitive address can not change
  // In each case, we first get a new value making use of getNewValues, as it has nothing to do with the keys
  // Then we call insertInStore with the analysis, the (old) key, the new key (generated by getNew...Addr), and the old and new value
  // insertInStore will handle cases where oldKey == newKey and/or oldValue == newValue
  def updateStore(a: IncrementalGlobalStore[Expression]): Unit =
    a.store.foreach((oldKey, oldValue) =>
      val newValue = getNewValues(a, oldValue)
      oldKey match
        case key: VarAddr =>
          insertInStore(a, key, getNewVarAddr(a, key), oldValue, newValue)
        case key: RetAddr =>
          insertInStore(a, key, getNewRetAddr(a, key), oldValue, newValue)
        case key: PtrAddr =>
          insertInStore(a, key, getNewPointerAddr(a, key), oldValue, newValue)
        case _ =>
    )

  // Much like with the store, we first get a new value. However, in this case the value is a set of SchemeModFComponent, so we use getNewComponent on each element in the set
  // The address dependency can once again be 4 different addresses, but unlike the store, now the value of a dependency of PrmAddr can change (but the PrmAddr itself does not)
  // For each, we call insertInDeps with the analysis, the address depencency, the new address dependency, the old and new value
  // We reuse functions such as getNewVarAddr to get the new addr dependency
  def updateDependencies(a: IncrementalModAnalysis[Expression]): Unit =
    a.deps.foreach((oldKey, oldValue) =>
      // Get a new set of values making use of getNewComponent
      val newValue = oldValue.map(e => getNewComponent(a, e))
      (oldKey, newValue) match
        case (addrDep: AddrDependency, newValue: Set[a.Component]) =>
          addrDep.addr match
            case k: VarAddr =>
              insertInDeps(a, addrDep, addrDep.copy(addr = getNewVarAddr(a, k)), oldValue, newValue)
            case k: RetAddr =>
              insertInDeps(a, addrDep, addrDep.copy(addr = getNewRetAddr(a, k)), oldValue, newValue)
            case k: PtrAddr =>
              insertInDeps(a, addrDep, addrDep.copy(addr = getNewPointerAddr(a, k)), oldValue, newValue)
            case k: PrmAddr =>
              insertInDeps(a, addrDep, addrDep, oldValue, newValue)
      )
  
  def buildNewExpr(expr: SchemeExp, allChanges: Map[maf.core.Expression, maf.core.Expression] = allExpressionsInChange): SchemeExp =
    allChanges.get(expr) match
      case Some(e) =>
        println(e)
        e.asInstanceOf[SchemeExp]
      case None => expr match
        case lambda: SchemeLambda       => SchemeLambda(lambda.name, lambda.args, lambda.body.map(buildNewExpr(_, allChanges)), lambda.annotation, lambda.idn)
        case lambda: SchemeVarArgLambda => SchemeVarArgLambda(lambda.name, lambda.args, lambda.vararg, lambda.body.map(buildNewExpr(_, allChanges)), lambda.annotation, lambda.idn)
        case let: SchemeLet             => SchemeLet(let.bindings.map(b => (b._1, buildNewExpr(b._2, allChanges))), let.body.map(buildNewExpr(_, allChanges)), let.idn)
        case let: SchemeLetStar         => SchemeLetStar(let.bindings.map(b => (b._1, buildNewExpr(b._2, allChanges))), let.body.map(buildNewExpr(_, allChanges)), let.idn)
        case let: SchemeLetrec          => SchemeLetrec(let.bindings.map(b => (b._1, buildNewExpr(b._2, allChanges))), let.body.map(buildNewExpr(_, allChanges)), let.idn)
        case ifExp: SchemeIf            => SchemeIf(buildNewExpr(ifExp.cond, allChanges), buildNewExpr(ifExp.cons, allChanges), buildNewExpr(ifExp.alt, allChanges), ifExp.idn)
        case fun: SchemeFuncall         => SchemeFuncall(buildNewExpr(fun.f, allChanges), fun.args.map(buildNewExpr(_, allChanges)), fun.idn)
        case set: SchemeSet             => SchemeSet(set.variable, buildNewExpr(set.value, allChanges), set.idn)
        case begin: SchemeBegin         => SchemeBegin(begin.exps.map(buildNewExpr(_, allChanges)), begin.idn)
        case scmVar: SchemeVar          => scmVar
        case _                          => expr


  // In the mapping, the key is a (Scheme) expression and the value is a set of components
  // So we once again want to update all the components, and update the key
  // the "new" key will be its equivalent in the allExpressionsInChange map or otherwise it will just be the old key
  def updateMapping(a: IncrementalModAnalysis[Expression]): Unit =
    a.mapping.foreach((oldKey, oldValue) =>
      val newValue = oldValue.map(e => getNewComponent(a, e))
      var newKey = oldKey
      allExpressionsInChange.get(oldKey) match
        case Some(nw) => newKey = nw
        case _ =>
          if findAllSubExps(oldKey).exists(e => allExpressionsInChange.contains(e)) then
            oldKey match
              case oldKey: SchemeExp => newKey = buildNewExpr(oldKey)
      (oldKey, newKey, newValue) match
        case (oldKey: SchemeExp, newKey: SchemeExp, newValue: Set[a.Component]) =>
          if newKey.equals(oldKey) then
            if !newValue.equals(oldValue) then
              a.mapping = a.mapping + (oldKey -> newValue)
          else
            a.mapping = a.mapping - oldKey
            a.mapping = a.mapping + (newKey -> newValue)
    )

  // Visited consists of a set of components
  // Fir this, we simply want to loop over this set and for each of them create a new component with getNewComponent
  // Because getNewComponent can return the original component, we test if the original and the new one are the same
  // If they are not, we replace the old component with the new component. Otherwise, nothing happens
  def updateVisited(a: IncrementalModAnalysis[Expression]): Unit =
    a.visited.foreach(comp =>
      getNewComponent(a, comp) match
      case newComp: a.Component =>
        if !newComp.equals(comp) then
          a.visited = a.visited - comp
          a.visited = a.visited + newComp
    )

  // Insert something in the store:
  //  if the old and new key are the same, but the value has changed
  //  or if there is a new key (in this case: remove the old key)
  def insertInStore(a: IncrementalGlobalStore[Expression], oldKey: Address, newKey: Address, oldValue: a.Value, newValue: a.Value): Unit =
    if newKey.equals(oldKey) then
      if !newValue.equals(oldValue) then
        a.store = a.store + (oldKey -> newValue)
    else
      a.store = a.store - oldKey
      a.store = a.store + (newKey -> newValue)

  // Insert something in the dependencies:
  //  if the old and new key are the same, but the value has changed
  //  or if there is a new key (in this case: remove the old key)
  def insertInDeps(a: IncrementalModAnalysis[Expression], oldKey: maf.modular.Dependency, newKey: maf.modular.Dependency, oldValue: Set[a.Component], newValue: Set[a.Component]): Unit =
    if newKey.equals(oldKey) then
      if !newValue.equals(oldValue) then
        a.deps = a.deps + (oldKey -> newValue)
    else
      a.deps = a.deps - oldKey
      a.deps = a.deps + (newKey -> newValue)


  // A variable address can only change if the variable exists somewhere in the changed expression
  // In this case, get what the variable has changed into and use that to create the new address
  // Otherwise, just return the old address
  def getNewVarAddr(a: IncrementalModAnalysis[Expression], addr: VarAddr): VarAddr =
    var newCtx = updateCtx(a, addr.ctx).asInstanceOf[addr.ctx.type]
    if changedVars contains addr.id then
      val newIdn = changedVars.getOrElse(addr.id, addr.id)
      val newAddr = addr.copy(id = newIdn, ctx = newCtx)
      newAddr
    else
      val newAddr = addr.copy(id = addr.id, ctx = newCtx)
      newAddr


  // Together with the idn of the return address that can change, it contains a component that might need to change
  // First, we look if the key's component is a function call. If it is, we want to find if the lambda of the component exists within a changed expression
  // If it is indeed a lambda that needs changing, allExpressionsInChange.get(lam) will return Some(lambda), otherwise it will return None.
  // A new component is created making use of getNewComponent
  // If the lambda does not change, we just return the old address
  // The call might also be main, we just return the address
  def getNewRetAddr(a: IncrementalModAnalysis[Expression], addr: RetAddr): RetAddr =
    addr.cmp match
      case SchemeModFComponent.Main =>
        addr
      case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
        val changeToLambda = allExpressionsInChange.get(lam)
        val newCtx = updateCtx(a, oldCtx).asInstanceOf[oldCtx.type]
        val newEnv = createNewEnvironment(lam, a, env)
        val newCmp = getNewComponent(a, SchemeModFComponent.Call((lam, BasicEnvironment[Address](newEnv)), newCtx))
        changeToLambda match
          case Some(lambda: SchemeLambdaExp) =>
            val newIdn = lambda.body.head.idn
            val newAddr = maf.modular.ReturnAddr[SchemeModFComponent](idn = newIdn, cmp = newCmp)
            newAddr
          case _ =>
            if findAllSubExps(lam).exists(e => allExpressionsInChange.contains(e)) then
              return maf.modular.ReturnAddr[SchemeModFComponent](idn = buildNewExpr(lam).asInstanceOf[SchemeLambda].body.head.idn, cmp = newCmp)
            val newAddr = maf.modular.ReturnAddr[SchemeModFComponent](idn = addr.idn, cmp = newCmp)
            newAddr


  // Get a new component. First look if it is a Main call or a function call. In case of main, just return the old component
  // In the case of a function call, only change the component if it exists within a changed expression (otherwise return the old component)
  // Also create a new environment making use of createNewEnvironment
  def getNewComponent(a: IncrementalModAnalysis[Expression], comp: Serializable): SchemeModFComponent =
    comp match
      case comp: SchemeModFComponent.Main.type =>
        comp
      case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), ctx: _) =>
        val changeToLambda = allExpressionsInChange.get(lam)
        val newCtx = updateCtx(a, ctx).asInstanceOf[ctx.type]
        changeToLambda match
          case Some(lambda: SchemeLambdaExp) =>
            val newEnv = createNewEnvironment(lambda, a, env)
            val newCmp = SchemeModFComponent.Call(clo = (lambda, new BasicEnvironment[Address](newEnv)), ctx = newCtx)
            newCmp
          case _ =>
            var newLam = lam
            if findAllSubExps(lam).exists(e => allExpressionsInChange.contains(e)) then
              newLam = buildNewExpr(lam).asInstanceOf[SchemeLambda]
            val newEnv = createNewEnvironment(lam, a, env)
            val newCmp = SchemeModFComponent.Call(clo = (newLam, new BasicEnvironment[Address](newEnv)), ctx = newCtx)
            newCmp


  // See if the expression is an expression that exists within a change expression. If not, nothing needs to happen. If so, it now becomes the expression of the new version
  def getNewPointerAddr(a: IncrementalModAnalysis[Expression], addr: PtrAddr): PtrAddr =
    val changeToExp = allExpressionsInChange.get(addr.exp)
    val newCtx = updateCtx(a, addr.ctx).asInstanceOf[addr.ctx.type]
    changeToExp match
      case Some(newExp: SchemeExp) =>
        addr.copy(exp = newExp, ctx = newCtx)
      case _ =>
        if findAllSubExps(addr.exp).exists(e => allExpressionsInChange.contains(e)) then
          return addr.copy(exp = buildNewExpr(addr.exp), ctx = newCtx)
        addr.copy(ctx = newCtx)

  // A value can be either annotated elements or elements. In both cases, we want to get all the values within the elements and update each of them
  def getNewValues(a: IncrementalGlobalStore[Expression], value: Serializable): a.Value =
    value match
      case element: IncrementalSchemeTypeDomain.modularLattice.AnnotatedElements =>
        var newValues = element.values
        newValues= element.values.map(e => getNewValue(a, e))
        element.copy(values = newValues).asInstanceOf[a.Value]
        //IncrementalSchemeTypeDomain.modularLattice.AnnotatedElements(newValues, element.sources).asInstanceOf[a.Value]
      case element: IncrementalSchemeTypeDomain.modularLattice.Elements =>
        val newElems = element.vs.map(e => getNewValue(a, e))
        element.copy(vs = newElems).asInstanceOf[a.Value]
     // case element: Value =>
      //  getNewValue(a, element).asInstanceOf[a.Value]
      case _ =>
      //  println(value.getClass)
        value.asInstanceOf[a.Value]

  // If the value is a set of closures, we want to update both the lambda and enviroment within each closure (if necessary).
  // In case of a vector, we want to loop over each of the elements and update them each accordingly
  // If it is a set of pointers, each of the pointers might need updating. For this, getNewPointerAddr is used
  def getNewValue(a: IncrementalGlobalStore[Expression], value:  Value): Value =
    value match
      case clos : IncrementalSchemeTypeDomain.modularLattice.Clo =>
        var newClos: Set[IncrementalSchemeTypeDomain.modularLattice.schemeLattice.Closure] = clos.closures.map(closure =>
          allExpressionsInChange.get(closure._1) match // check if lambda is in a change expression
            case Some(lambda: SchemeLambdaExp) =>
              closure._2 match // update the environment of the lambda if it needs changing
                case env : maf.core.BasicEnvironment[_] =>
                  var newEnv = createNewEnvironment(closure._1, a, env)
                  (lambda, new BasicEnvironment[Address](newEnv))
            case _ =>
              var nwLam = closure._1
              if findAllSubExps(nwLam).exists(e => allExpressionsInChange.contains(e)) then
                nwLam = buildNewExpr(nwLam).asInstanceOf[SchemeLambda]
              closure._2 match // update the environment of the lambda if it needs changing
                case env : maf.core.BasicEnvironment[_] =>
                  println("closure: ")
                  println(closure._1)
                  println(nwLam)
                  println(nwLam.idn)
                  val newEnv = createNewEnvironment(closure._1, a, env)
                  (nwLam, new BasicEnvironment[Address](newEnv))
        )
        IncrementalSchemeTypeDomain.modularLattice.Clo(newClos)
      case vector: IncrementalSchemeTypeDomain.modularLattice.Vec =>
        val newElementsVector = vector.elements.map((k, vecelem) =>
          val nw = getNewValues(a, vecelem)
          (k, nw)
        )
        val newVector = IncrementalSchemeTypeDomain.modularLattice.Vec(size = vector.size, elements = newElementsVector.asInstanceOf[vector.elements.type])
        newVector
      case pointer: IncrementalSchemeTypeDomain.modularLattice.Pointer =>
        IncrementalSchemeTypeDomain.modularLattice.Pointer(pointer.ptrs.map(p => p match
          case pa: PtrAddr =>
            getNewPointerAddr(a, pa)
        ))
      case cons: IncrementalSchemeTypeDomain.modularLattice.Cons =>
        val newcar = getNewValues(a, cons.car).asInstanceOf[cons.car.type]
        val newcdr = getNewValues(a, cons.cdr).asInstanceOf[cons.cdr.type]
        IncrementalSchemeTypeDomain.modularLattice.Cons(newcar, newcdr)
      case _ =>
        value

  // To create an new enviroment, loop over the old enviroment
  // If a variable did not change, it can be added to the new environment
  // If it did change, the variable that it changed into needs to be added to the environment
  def createNewEnvironment(expr: SchemeLambdaExp, a: IncrementalModAnalysis[Expression], oldEnv: maf.core.BasicEnvironment[_]): Map[String, Address] =
    println("old env: ")
    println(oldEnv.content)
 //   expr.fv.foreach(e => println(e))
    var newEnv: Map[String, Address] = Map()
    var changingIf = false
    var varsToRemove: Set[String] = Set()
    println(allIfs.exists(e => findAllSubExps(expr).exists(s => e._1._1.eql(s))))
    println(expr.fv)
  //  println(allIfs.exists(e => e._1._1.eql(expr.body.head)))*/
    allIfs.find(e => findAllSubExps(expr).exists(s => e._1._1.eql(s))) match
      case Some((exprs, ids: List[Identifier], _)) =>
        println(exprs._1.fv)
        println(exprs._2.fv)
        changingIf = true
        varsToRemove = exprs._1.fv.diff(exprs._2.fv)
        ids.foreach(e =>
          val newAddr = maf.modular.scheme.VarAddr(e, None)
          newEnv = newEnv + (e.name -> newAddr)
        )
      case None =>
    oldEnv.content.foreach((k, v) =>
      v match
        case varAddr: VarAddr =>
          val oldIdn = varAddr.idn
          changedVars.find((k , v) => k.idn == oldIdn) match
            case Some(identifiers) =>
              val newVarAddr = getNewVarAddr(a, varAddr)
              newEnv += (identifiers._2.name -> newVarAddr)
            case _ =>
              val newCtx = updateCtx(a, varAddr.ctx).asInstanceOf[varAddr.ctx.type]
              val newVarAddr = varAddr.copy(ctx= newCtx) // bugfix for some context sensitive things, context might update even if the actual var addr does not
              if !changingIf || !varsToRemove.contains(k) then
                println("this case")
                println(varsToRemove)
                println(k)
                newEnv += (k -> newVarAddr)
        case _ =>
          if !changingIf || !varsToRemove.contains(k) then
            newEnv += (k -> v))
    println("new env: ")
    println(newEnv)
    newEnv

  // Update context. This currently supports SchemeModFNoSensitivity, SchemeModFFullArgumentCallSiteSensitivity, SchemeModFCallSiteSensitivity and SchemeModFFullArgumentSensitivity
  // Context can either be Some(context), None or just a context alone, and of type ArgContext, CallSiteContext or ArgCallSiteContext
  def updateCtx(a: IncrementalModAnalysis[Expression], ctx: Any): Any =
    a match
      case a: IncrementalGlobalStore[Expression] =>
        ctx match
          case Some(ctx: maf.modular.scheme.modf.ArgContext) =>
            Some(updateArgCtx(a, ctx))
          case ctx: maf.modular.scheme.modf.ArgContext =>
            updateArgCtx(a, ctx)
          case Some(ctx: maf.modular.scheme.modf.CallSiteContext) =>
            Some(updateCallSiteCtx(a, ctx))
          case ctx: maf.modular.scheme.modf.CallSiteContext =>
            updateCallSiteCtx(a, ctx)
          case Some(ctx: maf.modular.scheme.modf.ArgCallSiteContext) =>
            Some(updateArgCallSiteCtx(a, ctx))
          case ctx: maf.modular.scheme.modf.ArgCallSiteContext =>
            updateArgCallSiteCtx(a, ctx)
          case Some(ctx: NoContext.type) =>
            Some(ctx)
          case ctx: NoContext.type =>
            ctx
          case None =>
            ctx
          case _ =>
            //(ctx.getClass)
            ctx

  // Update the ArgContext: ArgContext has a set of values so for each of them we want to update them
  // These values are (Annotated)Elements so we can reuse getNewValues
  def updateArgCtx(a: IncrementalGlobalStore[Expression], ctx: maf.modular.scheme.modf.ArgContext): maf.modular.scheme.modf.ArgContext =
    val newValues = ctx.values.map(elements => elements match
      case elements: Serializable =>
        getNewValues(a, elements)
        )
    maf.modular.scheme.modf.ArgContext(newValues)

  //Update the CallSiteCtx: a CallSiteContext has a set of calls which is a List of Positions
  //So we look for each of these positions whether they exists in a changed expression and if they are, we change it to the position of the expression the original expression changed into
  def updateCallSiteCtx(a: IncrementalGlobalStore[Expression], ctx: maf.modular.scheme.modf.CallSiteContext): maf.modular.scheme.modf.CallSiteContext =
   val newCalls = ctx.calls.map(call => findNewPosition(call))
   ctx.copy(calls = newCalls)

  // To update the ArgCallSiteContext we must take into account the args, the call and the fn
  // the arguments are updated in the same way as the arguments (values) of the updateArgCtx
  // The call is also updated in the same way as the calls in updateCallSiteCtx
  // The function is also updated in the same way as a call
  def updateArgCallSiteCtx(a: IncrementalGlobalStore[Expression], ctx: maf.modular.scheme.modf.ArgCallSiteContext): maf.modular.scheme.modf.ArgCallSiteContext =
    val newArgs = ctx.args.map(elements => elements match
      case elements: Serializable =>
        getNewValues(a, elements)
    )
    val newCall = findNewPosition(ctx.call)
    val newFn = findNewPosition(ctx.fn)
    maf.modular.scheme.modf.ArgCallSiteContext(newFn, newCall, newArgs)

  def findNewPosition(oldPosition: Position.Position): Position.Position =
    allExpressionsInChange.find((k, v) => k.idn.pos.equals(oldPosition)) match
      case Some(oldPos, newPos) =>
        newPos.idn.pos
      case _ =>
        oldPosition



}
