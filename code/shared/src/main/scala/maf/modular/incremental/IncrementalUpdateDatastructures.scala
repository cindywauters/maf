package maf.modular.incremental

import maf.cli.runnables
import maf.core.{Address, BasicEnvironment, Expression, Identifier, Identity, Lattice}
import maf.modular.scheme.modf.{NoContext, SchemeModFComponent}
import maf.language.scheme.interpreter.ConcreteValues.{Addr, AddrInfo}
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo.VarAddr
import maf.language.scheme.interpreter.BaseSchemeInterpreter
import maf.language.scheme.{SchemeLambdaExp, *}
import maf.language.scheme.interpreter.ConcreteValues.Value.Clo
import maf.language.scheme.lattices.ModularSchemeLattice
import maf.lattice.interfaces.{BoolLattice, CharLattice, IntLattice, RealLattice, StringLattice, SymbolLattice}
import maf.modular.incremental.scheme.lattice.{IncrementalLattice, IncrementalModularSchemeLattice, IncrementalSchemeLattice}
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

  def findAllLambdas(expr: Expression): List[Expression] =
    expr match
      case _: SchemeLambdaExp => List(expr).appendedAll(expr.subexpressions.flatMap(e => findAllLambdas(e)))
      case _ =>
        if expr.subexpressions.isEmpty then
          List()
        else expr.subexpressions.flatMap(e => findAllLambdas(e))

  def updateStore(a: IncrementalGlobalStore[SchemeExp], changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): Unit =
    println(a.store)
    a.store.foreach((k, v) =>
      updateBasedOnKeysStore(a, k, v, changedVars, changedExpressions)
    )
    println(a.store)

  def updateBasedOnKeysStore(a: IncrementalGlobalStore[SchemeExp], key: Address, value: a.Value, changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): Unit =
    (key, value) match
      case (k: maf.modular.scheme.VarAddr[NoContext.type], _) =>
        updateVarAddr(a, k, value, changedVars)
        updateValue(a, k, value, changedVars, changedExpressions)
      case (k: maf.modular.ReturnAddr[SchemeModFComponent], _) =>
        updateReturnAddr(a, k, value, changedVars, changedExpressions)
      case _ =>

  def updateVarAddr(a: IncrementalGlobalStore[SchemeExp], key: maf.modular.scheme.VarAddr[NoContext.type], value: a.Value, changedVars: Map[Identifier, Identifier]): Unit =
    if changedVars contains key.id then
      val newIdn = changedVars.getOrElse(key.id, key.id)
      val newKey = key.copy(id = newIdn, ctx = key.ctx)
      a.store = a.store - key
      a.store = a.store + (newKey -> value)


  def updateReturnAddr(a: IncrementalGlobalStore[SchemeExp], key: maf.modular.ReturnAddr[SchemeModFComponent], value: a.Value, changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): Unit =
    key.cmp match
      case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), ctx: NoContext.type) =>
        val allOldLambdas = changedExpressions.flatMap(e => findAllLambdas(e._1))
        val allNewLambdas = changedExpressions.flatMap(e => findAllLambdas(e._2))
        val allChangedLambdas = allOldLambdas.zip(allNewLambdas).toMap
       /* println()
        println(allChangedLambdas)
        println()*/
        val changeToLambda = allChangedLambdas.getOrElse(lam, false)
        changeToLambda match
          case lambda: SchemeLambdaExp =>
            val newIdn = lambda.subexpressions.last.idn
            var newEnv: Map[String, Address] = Map()
            env.content.foreach((k, v) =>
              v match
                case varAddr: maf.modular.scheme.VarAddr[NoContext.type] =>
                  var oldIdn = varAddr.idn
                  (changedVars.find((k , v) => k.idn == oldIdn)) match
                    case Some(identifiers) =>
                      newEnv += (identifiers._2.name -> varAddr.copy(id = identifiers._2))
                    case _ => newEnv += (k -> v)
                case _ => newEnv += (k -> v)
            )
            val newCmp = SchemeModFComponent.Call(clo = (lambda, new BasicEnvironment[Address](newEnv)), ctx = ctx)
            val newKey = key.copy(idn = newIdn, cmp = newCmp)
            a.store = a.store - key
            a.store = a.store + (newKey -> value)
          case false =>
      case _ =>

  def updateValue(a: IncrementalGlobalStore[SchemeExp], key: Address, v: a.Value, changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): Unit =
   a.lattice match {
     case l: IncrementalSchemeLattice[_, _]  =>
       var clos = l.getClosures(v)
       clos.map(e =>
         if changedExpressions.contains(e._1) then
           changedExpressions.getOrElse(e._1, e._1) match
             case lambda: SchemeLambdaExp =>
               a.store = a.store + (key -> l.closure(lambda, e._2))
             case _ =>
        // changedVars.find((k , v) => k.idn == key.idn) match
        //   case Some(identifiers) =>
         val allOldLambdas = changedExpressions.flatMap(e => findAllLambdas(e._1))
         val allNewLambdas = changedExpressions.flatMap(e => findAllLambdas(e._2))
         val allChangedLambdas = allOldLambdas.zip(allNewLambdas).toMap
         //if allChangedLambdas contains e._1 then
         (allChangedLambdas.get(e._1), changedVars.find((k , v) => k.idn == key.idn), key) match
           case (Some(lambda: SchemeLambdaExp), Some(identifiers), k: maf.modular.scheme.VarAddr[NoContext.type]) =>
             println(lambda.idn)
             println(changedVars.find((k , v) => k.idn == key.idn))
             println(l.closure(lambda, e._2))
             println(k)
             a.store = a.store + (k.copy(id = identifiers._2) -> l.closure(lambda, e._2))
         //    println(oldLambda)
             //a.store = a.store + (key -> l.closure(lambda, e._2))
           case _ =>
       )
     case _ => println(a.lattice)

   }
}
