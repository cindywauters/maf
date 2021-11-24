package maf.modular.incremental

import maf.cli.runnables
import maf.core.{Expression, Identifier, Identity}
import maf.modular.scheme.modf.SchemeModFComponent
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo.VarAddr
import maf.language.scheme.interpreter.BaseSchemeInterpreter
import maf.language.scheme.*

class IncrementalUpdateDatastructures {

 def changeDataStructures(a: IncrementalModAnalysis[SchemeExp], exp: SchemeExp): Boolean =

   var changed = SchemeChangePatterns.checkForRenamingParameter(exp)
   var changedParameters = changed.flatMap(e => e._2)
   println(changedParameters)
   var ChangedVarsSwapped = changedParameters.map(_.swap).toMap
   println(ChangedVarsSwapped)

   var changedExpressions = changed.map(e => e._1)
   changedExpressions.foreach(e => println(e._1.idn))

   a match
     case analysis: IncrementalGlobalStore[SchemeExp] =>
       println(analysis.store)

       var toChangeVars = ChangedVarsSwapped.flatMap(e => analysis.store.filter((k, v) => k.idn == e._1.idn))
       toChangeVars.foreach((k, v) => k match
         case key: maf.modular.scheme.VarAddr[VarAddr] =>
           println(VarAddr(ChangedVarsSwapped.getOrElse(key.id, key.id)))
           val oldValue = analysis.store.getOrElse(key, v)
           analysis.store = analysis.store - key
           val newIdn = ChangedVarsSwapped.getOrElse(key.id, key.id)
           val newKey = key.copy(id = newIdn)
           println(newKey)
           analysis.store = analysis.store + (newKey -> oldValue)
         case something: _ => println(something.getClass()))

       println(analysis.store)

       analysis.store.foreach((k, v) => k match
         case key: maf.modular.ReturnAddr[SchemeModFComponent] =>
           key.cmp match
             case SchemeModFComponent.Call(clo, ctx) =>
               println(key.idn.toString + " " + key.cmp.toString + " " + clo._1.toString + " " + clo._2.toString + ctx.toString)
               val changeToLambda = changedExpressions.toMap.getOrElse(clo._1, false)
               changeToLambda match
                 case lambda: SchemeLambdaExp =>
                   val newIdn = lambda.subexpressions.last.idn
                   val newCmp = SchemeModFComponent.Call(clo = (lambda, clo._2), ctx = ctx)
                   val newKey = key.copy(idn = newIdn, cmp = newCmp)
                   println(key.toString + " " + newKey.toString)
                   analysis.store = analysis.store - key
                   analysis.store = analysis.store + (newKey -> v)
                 case false =>
             case _ =>
         case _ => )
       var toChangeReturns = changedExpressions.map(e => analysis.store.filter((k, v) => k.idn == e._1.idn))
       println(analysis.store)
   true

}
