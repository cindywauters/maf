package maf.modular.incremental.update

import maf.core.{ArityError, BasicEnvironment, Monad, MonadJoin, VarArityError}
import maf.core.Position.Position
import maf.language.scheme.{SchemeExp, SchemeLambda, SchemeLambdaExp, SchemeVarArgLambda}
import maf.modular.incremental.scheme.modf.IncrementalSchemeModFBigStepSemantics
import maf.language.change.CodeVersion.*
import maf.modular.scheme.modf.SchemeModFComponent.Call
import maf.modular.scheme.modf.{BigStepModFSemantics, SchemeModFComponent}

trait UpdateIncrementalSchemeModFBigStepSemantics extends IncrementalSchemeModFBigStepSemantics  with IncrementalModAnalysisWithUpdateTwoVersions[SchemeExp]:

    trait UpdateIncrementalSchemeModFBigStepIntra extends IncrementalSchemeModFBigStepIntra:
        override protected def eval(exp: SchemeExp): EvalM[Value] = exp match
            case e if version == Old =>
                registerComponent(e, component)
                super.eval(e)
            case e if version == New =>
                registerComponent(e, component)
                super.eval(e)
