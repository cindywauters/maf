package maf.modular.incremental

import maf.cli.runnables
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo
import maf.language.scheme.interpreter.ConcreteValues.AddrInfo.VarAddr
import maf.language.scheme.interpreter.BaseSchemeInterpreter
import maf.language.scheme.{SchemeChangePatterns, SchemeChangeRenamer, SchemeExp}

class IncrementalUpdateDatastructures {

 def changeDataStructures(a: IncrementalModAnalysis[SchemeExp], exp: SchemeExp): Boolean =
   var changedParameters = SchemeChangePatterns.checkForRenamingParameter(exp).flatMap(e => e._2)
   println(changedParameters)
   var ChangedParamsSwapped = changedParameters.map(_.swap).toMap
   println(ChangedParamsSwapped)
   a match
     case analysis: IncrementalGlobalStore[SchemeExp] =>
       println(analysis.store)
       var toChange = ChangedParamsSwapped.flatMap(e => analysis.store.filter((k, v) => k.idn == e._1.idn))
       toChange.foreach((k, v) => k match
         case vr: maf.modular.scheme.VarAddr[VarAddr] =>
           val oldValue = analysis.store.getOrElse(vr, v)
           analysis.store = analysis.store - vr
           val newKey = vr.copy(id = ChangedParamsSwapped.getOrElse(vr.id, vr.id))
           analysis.store = analysis.store + (newKey -> oldValue)
         case something: _ => println(something.getClass()))
       println(analysis.store)
   true

}
