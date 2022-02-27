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
        allChanges.get(e) match
          case Some(newexp: SchemeExp) =>
            registerComponent(newexp, component)
            super.eval(newexp)
          case None =>
        /*    if update.findAllSubExps(e).exists(e => allChanges.contains(e)) then
              val newexp = update.buildNewExpr(e, allChanges)
              println("new exp")
              println(e)
              println(newexp)
              component match
                case SchemeModFComponent.Call((lam: SchemeLambdaExp, env: BasicEnvironment[_]), oldCtx: _) =>
                  println("component")
                  println(newexp)
                  println(lam)
                  println(env)
                  if e.eql(lam) then
                    println("here")
                    registerComponent(newexp, SchemeModFComponent.Call((newexp.asInstanceOf[SchemeLambda], env), oldCtx).asInstanceOf[Component])
              registerComponent(newexp, component)
              super.eval(newexp)
            else*/
              registerComponent(e, component)
              super.eval(e)