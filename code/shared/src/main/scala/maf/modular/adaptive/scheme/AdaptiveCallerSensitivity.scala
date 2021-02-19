package maf.modular.adaptive.scheme

import maf.modular.scheme._
import maf.modular.scheme.modf._
import maf.modular.scheme.modf.SchemeModFComponent._
import maf.core.Position._
import maf.language.scheme._

trait AdaptiveCallerSensitivity extends AdaptiveSchemeModFSemantics {
  // set a limit for the maximum number of recursive calls to a component
  val limit: Int
  // the context is just the calling component
  type ComponentContext = (Component, Position)
  def updateCtx(update: Component => Component)(ctx: ComponentContext) =
    (update(ctx._1), ctx._2)
  // adapting the caller argument
  def adaptCaller(
      clo: lattice.Closure,
      caller: Component,
      position: Position
    ): ComponentContext = ???
  def allocCtx(
      nam: Option[String],
      clo: lattice.Closure,
      args: List[Value],
      call: Position,
      caller: Component
    ) =
    adaptCaller(clo, caller, call)
  override def onNewComponent(cmp: Component, call: Call[ComponentContext]) = ???
  protected def adaptCall(cmp: Call[ComponentContext]): Call[ComponentContext] = cmp match {
    case Call(clo, nam, ctx) => Call(clo, nam, adaptCaller(clo, ctx._1, ctx._2))
  }
  def registerCall(source: (Component, Position), target: Component) = ???
  // we instrument the intra-analysis to perform the necessary bookkeeping for 'calledBy' whenever a function is called
  override def intraAnalysis(cmp: Component): AdaptiveSchemeModFAnalysisIntra = new AdaptiveSchemeModFAnalysisIntra(cmp)
  class AdaptiveSchemeModFAnalysisIntra(component: Component) extends AdaptiveSchemeModFIntra(component) {
    var currentPos: Position = null
    override def applyClosures(
        fun: Value,
        args: List[(SchemeExp, Value)],
        cll: Position
      ) = {
      this.currentPos = cll
      super.applyClosures(fun, args, cll)
    }
    override def call(cmp: Component) = {
      registerCall((component, currentPos), cmp)
      super.call(cmp)
    }
  }
}
