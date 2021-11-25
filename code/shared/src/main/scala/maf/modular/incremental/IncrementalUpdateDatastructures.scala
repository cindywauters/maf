package maf.modular.incremental

import maf.cli.runnables
import maf.core.{Address, Expression, Identifier, Identity}
import maf.modular.scheme.modf.{NoContext, SchemeModFComponent}
import maf.language.scheme.interpreter.ConcreteValues.{Addr, AddrInfo}
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo.VarAddr
import maf.language.scheme.interpreter.BaseSchemeInterpreter
import maf.language.scheme.*
import maf.modular.incremental.scheme.lattice.IncrementalModularSchemeLattice
import maf.modular.scheme.SchemeAddr


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

 def updateStore(a: IncrementalGlobalStore[SchemeExp], changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): Unit =
   println(a.store)
   a.store.foreach((k, v) =>
     updateBasedOnKeysStore(a, k, v, changedVars, changedExpressions)
   )
   println(a.store)

 def updateBasedOnKeysStore(a: IncrementalGlobalStore[SchemeExp], key: Address, value: a.Value, changedVars: Map[Identifier, Identifier], changedExpressions: Map[Expression, Expression]): Unit =
   key match
     case k: maf.modular.scheme.VarAddr[NoContext.type] =>
       updateVarAddr(a, k, value, changedVars)
     case k: maf.modular.ReturnAddr[SchemeModFComponent] =>
       updateReturnAddr(a, k, value, changedExpressions)
     case _ =>

  def updateVarAddr(a: IncrementalGlobalStore[SchemeExp], key: maf.modular.scheme.VarAddr[NoContext.type], value: a.Value, changedVars: Map[Identifier, Identifier]): Unit =
    if changedVars contains key.id then
      val newIdn = changedVars.getOrElse(key.id, key.id)
      val newKey = key.copy(id = newIdn, ctx = key.ctx)
      a.store = a.store - key
      a.store = a.store + (newKey -> value)

  def updateReturnAddr(a: IncrementalGlobalStore[SchemeExp], key: maf.modular.ReturnAddr[SchemeModFComponent], value: a.Value, changedExpressions: Map[Expression, Expression]): Unit =
    key.cmp match
      case SchemeModFComponent.Call(clo, ctx: NoContext.type) =>
        val changeToLambda = changedExpressions.toMap.getOrElse(clo._1, false)
        changeToLambda match
          case lambda: SchemeLambdaExp =>
            val newIdn = lambda.subexpressions.last.idn
            val newCmp = SchemeModFComponent.Call(clo = (lambda, clo._2), ctx = ctx)
            val newKey = key.copy(idn = newIdn, cmp = newCmp)
            a.store = a.store - key
            a.store = a.store + (newKey -> value)
          case false =>
      case _ =>


}
