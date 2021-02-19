package maf.web

import maf.core._
import maf.modular.GlobalStore
import maf.modular.adaptive._

// Scala.js-related imports
import scala.scalajs.js

object WebVisualisationAdaptive {
  val d3 = js.Dynamic.global.d3
  lazy val __NODE_COLORS__ = List("blue", "green", "yellow", "red")
  lazy val __NO_OF_COLORS__ = __NODE_COLORS__.length
  lazy val __COLOR_SCALE__ = d3
    .scaleOrdinal()
    .domain(d3.range(__NO_OF_COLORS__))
    .range(__NODE_COLORS__)
}

trait WebAdaptiveAnalysis[Expr <: Expression] extends AdaptiveModAnalysis[Expr] with GlobalStore[Expr] {
  var webvis: WebVisualisationAdaptive = null

  override def updateAnalysisData(update: Map[Component, Component]) = {
    super.updateAnalysisData(update)
    webvis.adapted = true
  }

  def key(cmp: Component): Any
}

class WebVisualisationAdaptive(override val analysis: WebAdaptiveAnalysis[_]) extends WebVisualisation(analysis) {

  override def componentKey(cmp: analysis.Component) = analysis.key(cmp)

  // give the analysis a pointer to the visualisation
  analysis.webvis = this

  var adapted = false

  override def componentText(cmp: analysis.Component) =
    s"[$cmp] ${analysis.deref(cmp).toString()}"

  override def refreshDataAfterStep(cmp: analysis.Component, dps: Set[analysis.Component]) =
    if (this.adapted) {
      this.adapted = false
      super.refreshData()
    } else {
      super.refreshDataAfterStep(cmp, dps)
    }
}
