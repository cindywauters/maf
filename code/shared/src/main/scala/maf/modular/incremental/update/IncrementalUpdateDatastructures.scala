package maf.modular.incremental.update

//import maf.cli.runnables
import maf.aam.scheme.SchemeStoreAllocateReturn
import maf.core.*
import maf.language.scheme.*
import maf.language.scheme.interpreter.BaseSchemeInterpreter
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo.VarAddr
import maf.language.scheme.interpreter.ConcreteValues.Value.Clo
import maf.language.scheme.interpreter.ConcreteValues.{Addr, AddrInfo}
import maf.lattice.ConstantPropagation
import maf.language.scheme.lattices.{ModularSchemeLattice, SchemeLattice, SchemeOp}
import maf.lattice.Type
import maf.lattice.interfaces.*
import maf.modular.{AddrDependency, ReturnAddr}
import maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice.incrementalSchemeLattice
import maf.modular.incremental.scheme.lattice.{IncrementalModularSchemeDomain, IncrementalModularSchemeLattice, IncrementalModularSchemeLatticeWrapper, *}
import maf.modular.incremental.{IncrementalGlobalStore, IncrementalModAnalysis}
import maf.modular.scheme.SchemeTypeDomain.primitives.allPrimitives
import maf.modular.scheme.modf.{NoContext, SchemeModFComponent}
import maf.modular.scheme.{ModularSchemeLatticeWrapper, PrmAddr, SchemeAddr}
import maf.language.scheme.primitives.SchemePrelude
import maf.modular.incremental.scheme.lattice

import scala.collection.mutable


class IncrementalUpdateDatastructures {

  type VarAddr = maf.modular.scheme.VarAddr[_]
  type RetAddr = maf.modular.ReturnAddr[_]
  type PtrAddr = maf.modular.scheme.PtrAddr[_]
  type PrmAddr = maf.modular.scheme.PrmAddr
  type TypeValue = maf.modular.incremental.scheme.lattice.IncrementalSchemeTypeDomain.modularLattice.Value
  type CPValue = maf.modular.incremental.scheme.lattice.IncrementalSchemeConstantPropagationDomain.modularLattice.Value


  var changedVars: Map[Identifier, Identifier] = Map()
  var changedExpressions: Map[Expression, Expression] = Map()
  var allExpressionsInChange: Map[Expression, Expression] = Map()
  var allIfs: IfsList = List()
  var ifsWithMappings: Map[Identifier, Set[SchemeModFComponent]] = Map()
  var allScopeChanges: ScopeChanges = Map()
 // var allExprs: List[Expression] = List()
  var initialEnvNew: Map[String, Identifier] = Map()
  var cachedComponents: mutable.LongMap[SchemeModFComponent] = mutable.LongMap()
  var unchangedComponents: Set[Int] = Set()
  var equivalentLambdas: Map[Expression, Expression] = Map()
  var allNewLexicalEnvs: BindingsWithScopes = Map()
  var lexEnvsBuildEnvs: mutable.LongMap[Map[String, Address]] = mutable.LongMap()
  var notToUpdate: List[Expression] = List()
  var toAddComponents: Set[SchemeModFComponent] = Set()
  var withRefactoring = true
  var cachedValues: mutable.LongMap[TypeValue] = mutable.LongMap()

  // Call this function when you want to update all the datastructures of an analysis
  // Arguments are an analysis and the expression that is being analysed
  def changeDataStructures(a: IncrementalModAnalysis[Expression], exp: List[Expression], renamings: List[((Expression, Expression), Map[Identifier, Identifier])], ifs: IfsList = List(), scopeChanges: ScopeChanges = Map(), otherChanges: List[(Expression, Expression)] = List(), allLexicalEnvs: BindingsWithScopes = Map(), doNotUpdate: List[Expression] = List()): Boolean =
    val changedVarsSwapped = renamings.flatMap(e => e._2).toMap
    val finder = new SchemeChangePatterns
    //val scopeChangedVars = scopeChanges.flatMap((k, v) => List((k._2._1, v._2._1)).appendedAll(finder.findAllVarsInOrder(k._1).zip(finder.findAllVarsInOrder(v._1))))
    val scopeChangedVars = scopeChanges.flatMap((k, v) => List((k._2._1, v._2._1)).appendedAll(finder.findAllVarsInOrder(k._1).zip(finder.findAllVarsInOrder(v._1))))
    changedVars = (changedVarsSwapped.map(_.swap) ++ scopeChangedVars).filter((k, v) => k != v) // Get all renamed vars
    var scopeChangesExprs: List[Expression] = scopeChanges.map((k, v) => (k._1, v._1)).flatMap(e => findAllSubExps(e._2)).collect {
      case e: SchemeLambdaExp => e.asInstanceOf[Expression]
    }.toList
    changedExpressions = renamings.map(e => e._1).toMap ++ scopeChanges.map((k, v) => (k._1, v._1)) // TODO fix this (same as above) // Get all expressions that have been changed
    allScopeChanges = scopeChanges

    allNewLexicalEnvs = allLexicalEnvs

 //   allExprs = exp

    notToUpdate = doNotUpdate
    withRefactoring = scopeChangesExprs.nonEmpty || renamings.nonEmpty || ifs.nonEmpty

    // get all expressions that exist within an old expression and in a new expression and zip them together to know what has changed to what
    val allOldExps = changedExpressions.flatMap(e => findAllSubExps(e._1)).toList//.appendedAll(otherChanges.map(_._1))
    val allNewExps = changedExpressions.flatMap(e => findAllSubExps(e._2)).toList//.appendedAll(otherChanges.map(_._2))
    allExpressionsInChange = allOldExps.zip(allNewExps).appendedAll(otherChanges).toMap
    equivalentLambdas = otherChanges.toMap

    ifs.foreach(i =>
      changedVars = changedVars ++ finder.findAllVarsInOrder(i._1._1.cond).zip(finder.findAllVarsInOrder(i._1._2.cond)).toMap
      changedVars = changedVars ++ finder.findAllVarsInOrder(i._1._1.cons).zip(finder.findAllVarsInOrder(i._1._2.alt)).toMap
      changedVars = changedVars ++ finder.findAllVarsInOrder(i._1._1.alt).zip(finder.findAllVarsInOrder(i._1._2.cons)).toMap)
  //  println("ALL EXPRESSIONS IN CHANGE")
  //  allExpressionsInChange.foreach(println)

    if exp.size > 1 then
      exp(1) match
        case letrec: SchemeLetrec =>
          initialEnvNew = letrec.bindings.map(e => (e._1.name, e._1)).toMap
          letrec.bindings.foreach(b => b._2 match
            case nestedLet: SchemeLettishExp =>
              nestedLet.bindings.foreach(nb => initialEnvNew = initialEnvNew + (nb._1.name -> nb._1))
            case _ =>
          )

    /** Step necessary for the mapping when scope changes are present
     * For example:
     *  Old: (lambda () (let ((f (lambda () 1))) (f))
     *  New: (lambda () (let () (f))
     * In this case the other changes will contain that let, which also should be changed to each other if reanalysis wants to be avoided
     * Therefore we get all subexpressions that are lettisch/lambda expressions and match them together if they have the same idn
     **/

    if scopeChanges.nonEmpty then
      var allSubsOtherOld: Set[Expression] = Set()
      var allSubsOtherNew: Set[Expression] = Set()
      otherChanges.map(_._1).sortWith(_.height > _.height).foreach(e => // Some optimisation, some of the other changes are nested lambdas so if the lambda is already present as a subexpression of another, its subexpressions will also already have been added
        if !allSubsOtherOld.contains(e) then //
          allSubsOtherOld = allSubsOtherOld ++ findAllSubExps(e).filter(e => e.isInstanceOf[SchemeLambdaExp] || e.isInstanceOf[SchemeLettishExp]))
      otherChanges.map(_._2).sortWith(_.height > _.height).foreach(e =>
        if !allSubsOtherOld.contains(e) then
          allSubsOtherNew = allSubsOtherNew ++ findAllSubExps(e).filter(e => e.isInstanceOf[SchemeLambdaExp] || e.isInstanceOf[SchemeLettishExp]))
      (for
        oldExp <- allSubsOtherOld
        newExp <- allSubsOtherNew
        if oldExp.idn == newExp.idn && oldExp.getClass == newExp.getClass //&& !allSubsOtherNew.contains(oldExp)// && (if oldExp.idn == NoCodeIdentity && oldExp.subexpressions.nonEmpty && newExp.subexpressions.nonEmpty then if oldExp.subexpressions.head.idn == newExp.subexpressions.head.idn then true else false else false)
      yield (oldExp, newExp)).foreach(e => allExpressionsInChange = allExpressionsInChange + (e._1 -> e._2))


    ifs.foreach(e =>
      val oldIf = e._1._1
    // val oldIfCondSubs = findAllSubExps(oldIf.cond)
      val newIf = e._1._2
    //  val newIfCondSubs = findAllSubExps(newIf.cond)
      findAllSubExps(e._3._1).zip(findAllSubExps(e._3._2)).foreach(e => allExpressionsInChange = allExpressionsInChange + (e._1 -> e._2))
      findAllSubExps(oldIf.cons).zip(findAllSubExps(newIf.alt)).foreach(e => allExpressionsInChange = allExpressionsInChange + (e._1 -> e._2))
      findAllSubExps(oldIf.alt).zip(findAllSubExps(newIf.cons)).foreach(e => allExpressionsInChange = allExpressionsInChange + (e._1 -> e._2))
      allExpressionsInChange = allExpressionsInChange + (oldIf -> newIf))

    allIfs = ifs

     ifsWithMappings = ifs.filter(i => i._2.nonEmpty).map(i =>
      a.mapping.get(i._3._1) match
        case Some(mapping: Set[SchemeModFComponent]) =>
          i._1._2.cond match
            case cond: SchemeFuncall =>
              a.mapping += (cond -> mapping)
              a.mapping += (cond.f -> mapping)
          (i._2.head, mapping)).toMap

    a match
      case analysis: IncrementalGlobalStore[Expression] => //if renamings.nonEmpty || ifs.nonEmpty || scopeChanges.nonEmpty => // Update the store
        updateStore(analysis)
      case _ =>
    updateDependencies(a) // Update the dependencies
    updateMapping(a) // Update the store
    updateVisited(a) // Update visited

    // Find which component the expression has moved to/ in which ones it belongs. Also keep track of where it no longer belongs
    var moved: Map[Expression, Set[SchemeModFComponent]] = scopeChangesExprs.map(e => (e, Set(): Set[SchemeModFComponent])).toMap
    if scopeChangesExprs.nonEmpty then
      a.visited.foreach(comp => comp match
        case comp @ SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), ctx: _) =>
          val allSubs = findAllSubExps(lam)
          scopeChangesExprs.foreach(expr =>
            if lam.equals(expr) then //allSubs.contains(exprs._2) then
              var allSubsExpr = findAllSubExps(expr).tail
              allSubsExpr.foreach(e =>
                moved = moved + (e -> (moved.getOrElse(e, Set()) ++ Set(comp))))
            else if allSubs.contains(expr) then
                moved = moved + (expr -> (moved.getOrElse(expr, Set()) ++ Set(comp)))
           // allSubs.filter(sub => a.mapping.contains(sub)).foreach(sub => moved = moved + (sub -> toInsert))
        )
        case _ =>
      )
    moved.foreach(e =>
     var toInsert: Set[a.Component] = Set(a.initialComponent)
     if e._2.nonEmpty then
       toInsert = e._2.asInstanceOf[Set[a.Component]]
     if !a.mapping.get(e._1).contains(toInsert) then
       a.mapping.find(m => m._1.idn == e._1.idn) match
         case Some(map) =>
           a.mapping = a.mapping + (e._1 -> toInsert)
         case _ =>
         )

    //List[((SchemeIf, SchemeIf),  List[Identifier], (Expression, Expression))]
    ifs.foreach(ifstatement =>
        if ifstatement._2.nonEmpty then
            var depVar = AddrDependency(maf.modular.scheme.VarAddr(ifstatement._2.head, None))
            var depRet = a.visited.collect{ case comp@SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) if lam.name.contains(ifstatement._2.head.name) => ReturnAddr(comp, lam.body.head.idn)}
            a.deps.get(depVar) match
                case Some(deps) =>
                    a.mapping.get(ifstatement._1._2) match
                        case Some(mappings) =>
                            a.deps = a.deps + (depVar -> (deps ++ mappings))
                            depRet.foreach(ret =>
                                a.deps = a.deps + (AddrDependency(ret) -> (deps ++ mappings)))
                case _ =>

    )

    toAddComponents.foreach(c =>
        a.addToWorkList(cachedComponents.getOrElse(c.hashCode().toLong, c).asInstanceOf[a.Component])
    )
    true

  // Find all the subexpressions of an expression, and their subexpressions.
  // Something like (lambda (a) (+ a 1)) will become List((lambda (a) (+ a 1)), (+ a 1), +, a, 1)
  def findAllSubExps(expr: Expression, upToHeight: Int = 1): List[Expression] =
    if expr.subexpressions.isEmpty && expr.height == upToHeight then
      List(expr)
    else if  expr.subexpressions.isEmpty then
      List()
    else expr match
      case lambda: SchemeLambda =>  List(expr).appended(SchemeBegin(lambda.body, lambda.body.head.idn)).appendedAll(expr.subexpressions.flatMap(e => findAllSubExps(e)))
      case _ => List(expr).appendedAll(expr.subexpressions.flatMap(e => findAllSubExps(e)))


  // Update the store in case there is one
  // There are three types of keys that can contain changes: Variable addresses, Return addresses, and Pointer Addresses. Primitive address can not change
  // In each case, we first get a new value making use of getNewValues, as it has nothing to do with the keys
  // Then we call insertInStore with the analysis, the (old) key, the new key (generated by getNew...Addr), and the old and new value
  // insertInStore will handle cases where oldKey == newKey and/or oldValue == newValue
  def updateStore(a: IncrementalGlobalStore[Expression]): Unit =
    a.store.foreach((oldKey, oldValue) =>
      oldKey match
        case key: VarAddr =>
          val newValue = getNewValues(Some(key), a, oldValue)
          insertInStore(a, key, getNewVarAddr(a, key), oldValue, newValue)
        case key: RetAddr =>//if withRefactoring =>
          val newValue = getNewValues(Some(key), a, oldValue)
          insertInStore(a, key, getNewRetAddr(a, key), oldValue, newValue)
        case key: PtrAddr if withRefactoring =>
          val newValue = getNewValues(Some(key), a, oldValue)
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
         //     if !refactoring && allNewLexicalEnvs.contains()
              insertInDeps(a, addrDep, addrDep.copy(addr = getNewRetAddr(a, k)), oldValue, newValue)
            case k: PtrAddr =>
              insertInDeps(a, addrDep, addrDep.copy(addr = getNewPointerAddr(a, k)), oldValue, newValue)
            case k: PrmAddr =>
              insertInDeps(a, addrDep, addrDep, oldValue, newValue)
      )

  def buildNewExpr(expr: SchemeExp, allChanges: Map[Expression, Expression] = allExpressionsInChange): SchemeExp =
    allChanges.get(expr) match
      case Some(e: SchemeExp) =>
        e
      case None if allNewLexicalEnvs.contains(expr) =>
        expr // In this case the expression hasn't changes at all as it also exists in the new expressions
      case None =>
        var newExpression = expr
        expr match
          case lambda: SchemeLambda       =>
            newExpression = SchemeLambda(lambda.name, lambda.args, lambda.body.map(buildNewExpr(_, allChanges)), lambda.annotation, lambda.idn)
          case lambda: SchemeVarArgLambda =>
            newExpression = SchemeVarArgLambda(lambda.name, lambda.args, lambda.vararg, lambda.body.map(buildNewExpr(_, allChanges)), lambda.annotation, lambda.idn)
          case let: SchemeLet             =>
            newExpression = SchemeLet(let.bindings.map(b => (b._1, buildNewExpr(b._2, allChanges))), let.body.map(buildNewExpr(_, allChanges)), let.idn)
          case let: SchemeLetStar         =>
            newExpression = SchemeLetStar(let.bindings.map(b => (b._1, buildNewExpr(b._2, allChanges))), let.body.map(buildNewExpr(_, allChanges)), let.idn)
          case let: SchemeLetrec          =>
            newExpression = SchemeLetrec(let.bindings.map(b => (b._1, buildNewExpr(b._2, allChanges))), let.body.map(buildNewExpr(_, allChanges)), let.idn)
          case ifExp: SchemeIf            =>
            newExpression = SchemeIf(buildNewExpr(ifExp.cond, allChanges), buildNewExpr(ifExp.cons, allChanges), buildNewExpr(ifExp.alt, allChanges), ifExp.idn)
          case fun: SchemeFuncall         =>
            newExpression = SchemeFuncall(buildNewExpr(fun.f, allChanges), fun.args.map(buildNewExpr(_, allChanges)), fun.idn)
          case set: SchemeSet             =>
            newExpression = SchemeSet(set.variable, buildNewExpr(set.value, allChanges), set.idn)
          case begin: SchemeBegin         =>
            newExpression = SchemeBegin(begin.exps.map(buildNewExpr(_, allChanges)), begin.idn)
          case scmVar: SchemeVar          =>
            newExpression = scmVar
          case _                          =>
        allExpressionsInChange = allExpressionsInChange + (expr -> newExpression) // cache for next time
        newExpression


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
          oldKey match
            case oldKey: SchemeExp => newKey = buildNewExpr(oldKey)
      (oldKey, newKey, newValue) match
        case (oldKey: SchemeExp, newKey: SchemeExp, newValue: Set[a.Component]) =>
          a.mapping = a.mapping - oldKey
          a.mapping = a.mapping + (newKey -> newValue)
    )

  // Visited consists of a set of components
  // For this, we simply want to loop over this set and for each of them create a new component with getNewComponent
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
      a.store = a.store + (oldKey -> newValue)
    else
      a.store = a.store - oldKey
      a.store = a.store + (newKey -> newValue)

  // Insert something in the dependencies:
  //  if the old and new key are the same, but the value has changed
  //  or if there is a new key (in this case: remove the old key)
  def insertInDeps(a: IncrementalModAnalysis[Expression], oldKey: maf.modular.Dependency, newKey: maf.modular.Dependency, oldValue: Set[a.Component], newValue: Set[a.Component]): Unit =
    if newKey.equals(oldKey) then
      a.deps = a.deps + (oldKey -> newValue)
    else
      a.deps = a.deps - oldKey
      a.deps = a.deps + (newKey -> newValue)

  // A variable address can only change if the variable exists somewhere in the changed expression
  // In this case, get what the variable has changed into and use that to create the new address
  // Otherwise, just return the old address
  def getNewVarAddr(a: IncrementalModAnalysis[Expression], addr: VarAddr): VarAddr =
    var newCtx = updateCtx(a, addr.ctx)
    if changedVars contains addr.id then
      val newIdn = changedVars.getOrElse(addr.id, addr.id)
      if allScopeChanges.nonEmpty && newCtx == None && !initialEnvNew.exists(e => e._2.idn == newIdn.idn) then
          newCtx = Some(NoContext)
      if allScopeChanges.nonEmpty && initialEnvNew.exists(e => e._2.idn == newIdn.idn) then
          newCtx = None
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
        val newCtx = updateCtx(a, oldCtx)
        val newEnv = createNewEnvironment(lam, a, env)
        val newCmp = getNewComponent(a, SchemeModFComponent.Call((lam, BasicEnvironment[Address](newEnv)), newCtx))
        changeToLambda match
          case Some(lambda: SchemeLambdaExp) =>
            val newIdn = lambda.body.head.idn
            val newAddr = maf.modular.ReturnAddr[SchemeModFComponent](idn = newIdn, cmp = newCmp)
            newAddr
          case _ =>
            val newAddr = maf.modular.ReturnAddr[SchemeModFComponent](idn = addr.idn, cmp = newCmp)
            newAddr


  // Get a new component. First look if it is a Main call or a function call. In case of main, just return the old component
  // In the case of a function call, only change the component if it exists within a changed expression (otherwise return the old component)
  // Also create a new environment making use of createNewEnvironment
  def getNewComponent(a: IncrementalModAnalysis[Expression], comp: Serializable): SchemeModFComponent =
    val compHash = comp.hashCode()
    if unchangedComponents.contains(compHash) then
        return comp.asInstanceOf[SchemeModFComponent]
    cachedComponents.get(comp.hashCode().toLong) match
      case Some(newComp) =>
        return newComp
      case _             =>
    comp match
      case comp: SchemeModFComponent.Main.type =>
        comp
      case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), ctx: _) =>
        val changeToLambda = allExpressionsInChange.get(lam)
        val newCtx = updateCtx(a, ctx)
        changeToLambda match
          case Some(lambda: SchemeLambdaExp) =>
            val newEnv = createNewEnvironment(lambda, a, env)
            val newCmp = SchemeModFComponent.Call(clo = (lambda, new BasicEnvironment[Address](newEnv)), ctx = newCtx)
            if newCmp.equals(comp) then
                unchangedComponents = unchangedComponents + compHash
            else cachedComponents = cachedComponents + (compHash.toLong -> newCmp)
            newCmp
          case _ =>
            val newEnv = createNewEnvironment(lam, a, env)
            val newCmp = SchemeModFComponent.Call(clo = (lam, new BasicEnvironment[Address](newEnv)), ctx = newCtx)
              if newCmp.equals(comp) then
                  unchangedComponents = unchangedComponents + compHash
              else cachedComponents = cachedComponents + (compHash.toLong -> newCmp)
            newCmp


  // See if the expression is an expression that exists within a change expression. If not, nothing needs to happen. If so, it now becomes the expression of the new version
  def getNewPointerAddr(a: IncrementalModAnalysis[Expression], addr: PtrAddr): PtrAddr =
    val changeToExp = buildNewExpr(addr.exp)
    val newCtx = updateCtx(a, addr.ctx)
   // changeToExp match
     // case Some(newExp: SchemeExp) =>
    addr.copy(exp = changeToExp, ctx = newCtx)
     // case _ =>
       // addr.copy(ctx = newCtx)
        //addr.copy(exp = buildNewExpr(addr.exp), ctx = newCtx)

  // A value can be either annotated elements or elements. In both cases, we want to get all the values within the elements and update each of them
  def getNewValues(key: Option[Address], a: IncrementalGlobalStore[Expression], value: Serializable): a.Value =
    value match
      case element: IncrementalSchemeTypeDomain.modularLattice.AnnotatedElements =>
        element.copy(values = element.values.map(e => getNewValueType(key, a, e))).asInstanceOf[a.Value]
      case element: IncrementalSchemeTypeDomain.modularLattice.Elements =>
        element.copy(vs = element.vs.map(e => getNewValueType(key, a, e))).asInstanceOf[a.Value]
      case element: IncrementalSchemeConstantPropagationDomain.modularLattice.AnnotatedElements =>
        element.copy(values = element.values.map(e => getNewValueCP(key, a, e))).asInstanceOf[a.Value]
      case element: IncrementalSchemeConstantPropagationDomain.modularLattice.Elements =>
        element.copy(vs = element.vs.map(e => getNewValueCP(key, a, e))).asInstanceOf[a.Value]
      case _ =>
        value.asInstanceOf[a.Value]

  def getNewClosure(a: IncrementalModAnalysis[Expression], lam: SchemeLambdaExp, env: Environment[Address], varAddr: Option[Address]): (SchemeLambdaExp, Environment[Address]) =
    var newEnv: Map[String, Address] = Map()
    env match // update the environment of the lambda if it needs changing
      case env : maf.core.BasicEnvironment[_] =>
        newEnv = createNewEnvironment(lam, a, env)
      if notToUpdate.contains(lam) && varAddr.isDefined then
        a.deps.get(AddrDependency(varAddr.get)) match
            case Some(deps: Set[SchemeModFComponent]) => toAddComponents = toAddComponents ++ deps
            case _ =>
      //(lam, new BasicEnvironment[Address](newEnv))
    //else
    allExpressionsInChange.get(lam) match // check if lambda is in a change expression
      case Some(lambda: SchemeLambdaExp) =>
        (lambda, new BasicEnvironment[Address](newEnv))
      case None if allNewLexicalEnvs.contains(lam) =>
        (lam, new BasicEnvironment[Address](newEnv))
      case _ =>
       // var nwLam = lam
       // if findAllSubExps(nwLam).exists(e => allExpressionsInChange.contains(e)) then
        var nwLam = buildNewExpr(lam).asInstanceOf[lam.type]
        (nwLam, new BasicEnvironment[Address](newEnv))

  // If the value is a set of closures, we want to update both the lambda and enviroment within each closure (if necessary).
  // In case of a vector, we want to loop over each of the elements and update them each accordingly
  // If it is a set of pointers, each of the pointers might need updating. For this, getNewPointerAddr is used
  def getNewValueType(key: Option[Address], a: IncrementalGlobalStore[Expression], value: TypeValue): TypeValue =
    value match
      case clos : IncrementalSchemeTypeDomain.modularLattice.Clo =>
        if clos.closures.size > 5 then
          cachedValues.get(value.hashCode().toLong) match
            case Some(newVal) => return newVal
            case None =>
        var newclos = IncrementalSchemeTypeDomain.modularLattice.Clo(clos.closures.map(clos => getNewClosure(a, clos._1, clos._2, key)))
        if newclos.closures.size > 5 then
            cachedValues = cachedValues + (value.hashCode().toLong -> newclos)
        newclos
      case vector: IncrementalSchemeTypeDomain.modularLattice.Vec =>
        cachedValues.get(value.hashCode().toLong) match
          case Some(newVal) => return newVal
          case None =>
        val newElementsVector = vector.elements.map((k, vecelem) =>
          (k, getNewValues(key, a, vecelem)))
        var newVec = IncrementalSchemeTypeDomain.modularLattice.Vec(size = vector.size, elements = newElementsVector.asInstanceOf[vector.elements.type])
        cachedValues = cachedValues + (value.hashCode().toLong -> newVec)
        newVec
      case pointer: IncrementalSchemeTypeDomain.modularLattice.Pointer =>
        cachedValues.get(value.hashCode().toLong) match
          case Some(newVal) => return newVal
          case None =>
        var newPointer = IncrementalSchemeTypeDomain.modularLattice.Pointer(pointer.ptrs.map(p => p match
          case pa: PtrAddr => getNewPointerAddr(a, pa)))
        cachedValues = cachedValues + (value.hashCode().toLong -> newPointer)
        newPointer
      case cons: IncrementalSchemeTypeDomain.modularLattice.Cons =>
        cachedValues.get(value.hashCode().toLong) match
          case Some(newVal) => return newVal
          case None =>
        val newcar = getNewValues(key, a, cons.car).asInstanceOf[cons.car.type]
        val newcdr = getNewValues(key, a, cons.cdr).asInstanceOf[cons.cdr.type]
        var newCons = IncrementalSchemeTypeDomain.modularLattice.Cons(newcar, newcdr)
        cachedValues = cachedValues + (value.hashCode().toLong -> newCons)
        newCons
      case _ =>
        value

  def getNewValueCP(key: Option[Address], a: IncrementalGlobalStore[Expression], value: CPValue): CPValue =
    value match
      case clos : IncrementalSchemeConstantPropagationDomain.modularLattice.Clo =>
        IncrementalSchemeConstantPropagationDomain.modularLattice.Clo(clos.closures.map(clos => getNewClosure(a, clos._1, clos._2, key)))
      case vector: IncrementalSchemeConstantPropagationDomain.modularLattice.Vec =>
        val newElementsVector = vector.elements.map((k, vecelem) =>
          (k, getNewValues(key, a, vecelem)))
        IncrementalSchemeConstantPropagationDomain.modularLattice.Vec(size = vector.size, elements = newElementsVector.asInstanceOf[vector.elements.type])
      case pointer: IncrementalSchemeConstantPropagationDomain.modularLattice.Pointer =>
        IncrementalSchemeConstantPropagationDomain.modularLattice.Pointer(pointer.ptrs.map(p => p match
          case pa: PtrAddr => getNewPointerAddr(a, pa)))
      case cons: IncrementalSchemeConstantPropagationDomain.modularLattice.Cons =>
        val newcar = getNewValues(key, a, cons.car).asInstanceOf[cons.car.type]
        val newcdr = getNewValues(key, a, cons.cdr).asInstanceOf[cons.cdr.type]
        IncrementalSchemeConstantPropagationDomain.modularLattice.Cons(newcar, newcdr)
      case _ =>
        value

  def createNewEnvironment(expr: SchemeLambdaExp, a: IncrementalModAnalysis[Expression], oldEnv: maf.core.BasicEnvironment[_]): Map[String, Address] =
    lexEnvsBuildEnvs.get((expr, oldEnv).hashCode().toLong) match
      case Some(env) => return env
      case _         =>
    val eqlLam = equivalentLambdas.find(l => l._1 == expr || l._2 == expr).getOrElse((expr, expr)).asInstanceOf[(SchemeExp, SchemeExp)]
    var newEnv: Map[String, Address] = Map()
    val newEnvIds: (Identifier, Map[String, Identifier]) = allNewLexicalEnvs.getOrElse(eqlLam._2, (Identifier("", NoCodeIdentity), Map()))
    eqlLam._2.fv.foreach(fv =>
      if allPrimitives.contains(fv) && !newEnvIds._2.contains(fv) && !oldEnv.content.contains(fv) then // Check for oldEnv is necessary if there is, for example, "vector" used as a variable rather than a primitive
        newEnv = newEnv + (fv -> PrmAddr(fv))
      else oldEnv.content.get(fv) match
        case Some(varAddr: VarAddr) =>
          val oldIdn = varAddr.idn
          changedVars.find((k , v) => k.idn == oldIdn) match
            case Some(identifiers) =>
              val newVarAddr = getNewVarAddr(a, varAddr)
                newEnv += (identifiers._2.name -> newVarAddr)
            case _ =>
              val newCtx = if allScopeChanges.nonEmpty && changedVars.exists(v => v._1.name == fv) && initialEnvNew.exists(e => e._2.idn == varAddr.idn.idn) then
                None
              else
                  val newCtx = updateCtx(a, varAddr.ctx)
                  if newCtx == None then
                      initialEnvNew = initialEnvNew + (varAddr.id.name -> varAddr.id)
                  newCtx
              val newVarAddr = varAddr.copy(ctx= newCtx) // bugfix for some context sensitive things, context might update even if the actual var addr does not
                newEnv = newEnv + (fv -> newVarAddr)
        case Some(prim: PrmAddr) =>
          newEnv = newEnv + (fv -> prim)
        case None =>
          newEnvIds._2.get(fv) match
            case Some(identifier: Identifier) =>
              var newCtx = if allScopeChanges.nonEmpty && initialEnvNew.exists(e => e._2.idn == identifier.idn) then
                None
              else
                oldEnv.content.find(e => e._2.idn == identifier.idn) match
                    case Some(s, oldVar: VarAddr) =>
                        val newCtx = updateCtx(a, oldVar.ctx)
                        if newCtx == None then
                            initialEnvNew = initialEnvNew + (oldVar.id.name -> oldVar.id)
                        newCtx
                    case _ =>
                        changedVars.find((k , v) => v.idn == identifier.idn) match
                            case Some(oldId) =>
                                oldEnv.content.get(oldId._1.name) match
                                    case Some(varAddr: VarAddr) =>
                                        varAddr.ctx
                            case None =>  if initialEnvNew.exists(e => e._2.idn == identifier.idn) then None else Some(NoContext)
              newEnv = newEnv + (fv -> maf.modular.scheme.VarAddr(identifier, newCtx))
            case None =>
              /*println(expr)
              println(fv)*/
    )
            //  throw new RuntimeException("please provide correct scopes to the environment builder"))
    lexEnvsBuildEnvs = lexEnvsBuildEnvs + ((expr, oldEnv).hashCode().toLong -> newEnv)
    newEnv

  // To create an new enviroment, loop over the old enviroment
  // If a variable did not change, it can be added to the new environment
  // If it did change, the variable that it changed into needs to be added to the environment
  /*def createNewEnvironment(expr: SchemeLambdaExp, a: IncrementalModAnalysis[Expression], oldEnv: maf.core.BasicEnvironment[_]): Map[String, Address] =
    var newEnv: Map[String, Address] = Map()
    //var changingIf = false
    val eqlLam = equivalentLambdas.find(l => l._1 == expr || l._2 == expr).getOrElse((expr, expr)).asInstanceOf[(SchemeExp, SchemeExp)]
    val subsOld = findAllSubExps(eqlLam._1)
    val subsNew = findAllSubExps(eqlLam._2)
    if eqlLam._1 != eqlLam._2 then
      allScopeChanges.find(changed => subsOld.contains(changed._1._1) && !subsNew.contains(changed._2._1)) match
        case Some(oldScope, newScope) =>
          //varsToRemove = eqlLam._1.fv.diff(eqlLam._2.fv)
          //TODO what if someone moved a function down within the same scope?
          if allExprs.size > 1 && allExprs(1).subexpressions.contains(newScope._1) then
            newEnv = newEnv + (newScope._2._1.name ->  maf.modular.scheme.VarAddr(newScope._2._1, None))
          else
            newEnv = newEnv + (newScope._2._1.name ->  maf.modular.scheme.VarAddr(newScope._2._1, Some(NoContext)))
        case _ =>

      allScopeChanges.find(changed => !subsOld.contains(changed._1._1) && subsNew.contains(changed._2._1)) match
        case Some(oldScope, newScope) =>
          eqlLam._2.fv.foreach(fv =>
            if allPrimitives.contains(fv) then
              newEnv = newEnv + (fv -> PrmAddr(fv))
            else
              newScope._2._2.get(fv) match
                case Some(identifier) =>
                  if allExprs.size > 1 && allExprs(1).subexpressions.contains(identifier) then
                    newEnv = newEnv + (identifier.name ->  maf.modular.scheme.VarAddr(identifier, None))
                  else
                    newEnv = newEnv + (identifier.name ->  maf.modular.scheme.VarAddr(identifier, Some(NoContext)))
                case _ =>)
        case _ =>

   // (allScopeChanges.map((k, v) => (k._1, v._1)) ++ allIfs.map(_._1).toMap).find(changed => subsOld.contains(changed._1) || subsNew.contains(changed._2)) match
     // case Some(exprs) =>
       // varsToRemove = eqlLam.//exprs._1.fv.diff(exprs._2.fv)
     /*   buildNewExpr(expr).fv.foreach(fv =>
          if allPrimitives.contains(fv) then
            newEnv = newEnv + (fv -> PrmAddr(fv)))
        allIfs.find(e => findAllSubExps(expr).exists(s => e._1._1.eql(s) || e._1._2.eql(s))) match
          case Some((exprs, ids: List[Identifier], _)) =>
            ids.foreach(e =>
              val newAddr = maf.modular.scheme.VarAddr(e, None)
              newEnv = newEnv + (e.name -> newAddr))
          case None =>
        allScopeChanges.find(e => findAllSubExps(expr).exists(s => e._1._1.eql(s) || e._2._1.eql(s))) match
          case Some(result) =>
            result._2._2._2.foreach(e =>
              if exprs._2.fv.contains(e._1) then
                val newAddr = maf.modular.scheme.VarAddr(e._2, None)
                  newEnv = newEnv + (e._1 -> newAddr))
          case None =>
      case None =>*/
    //val prims = new SchemeLatticePrimitives[ModularSchemeLattice.L, SimpleAddr]
    oldEnv.content.foreach((k, v) =>
      v match
        case varAddr: VarAddr =>
          val oldIdn = varAddr.idn
          changedVars.find((k , v) => k.idn == oldIdn) match
            case Some(identifiers) =>
              val newVarAddr = getNewVarAddr(a, varAddr)
              newEnv += (identifiers._2.name -> newVarAddr)
            case _ =>
              val newCtx = if allExprs.size > 1 &&  allExprs(1).subexpressions.exists(s => s.idn == varAddr.idn) then
                None
              else
                updateCtx(a, varAddr.ctx)
              val newVarAddr = varAddr.copy(ctx= newCtx) // bugfix for some context sensitive things, context might update even if the actual var addr does not
              newEnv += (k -> newVarAddr)
        case _ =>
          newEnv += (k -> v))
    newEnv*/

  // Update context. This currently supports SchemeModFNoSensitivity, SchemeModFFullArgumentCallSiteSensitivity, SchemeModFCallSiteSensitivity and SchemeModFFullArgumentSensitivity
  // Context can either be Some(context), None or just a context alone, and of type ArgContext, CallSiteContext or ArgCallSiteContext
  def updateCtx(a: IncrementalModAnalysis[Expression], ctx: Any): Any =
   // return ctx
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
        getNewValues(None, a, elements)
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
        getNewValues(None, a, elements)
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
