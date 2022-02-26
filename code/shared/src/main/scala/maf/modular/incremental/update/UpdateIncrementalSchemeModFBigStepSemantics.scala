package maf.modular.incremental.update

import maf.language.scheme.SchemeExp
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.language.change.CodeVersion.*

trait UpdateIncrementalSchemeModFBigStepSemantics extends IncrementalSchemeModFBigStepSemantics  with IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp]:

  trait UpdateIncrementalSchemeModFBigStepIntra extends IncrementalSchemeModFBigStepIntra:
    override protected def eval(exp: SchemeExp): EvalM[Value] = exp match
      case e if version == Old => super.eval(e)
      case e if version == New =>
        allChanges.get(e) match
          case Some(newexp: SchemeExp) => super.eval(newexp)
          case None => super.eval(e)
