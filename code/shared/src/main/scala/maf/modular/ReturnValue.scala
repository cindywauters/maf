package maf.modular

import maf.core._

case class ReturnAddr[Component](cmp: Component, idn: Identity) extends Address:
    def printable = true
    override def toString: String = s"ret ($cmp)"

/**
 * Provides facilities for storing and retrieving return values of components (component analyses).
 * @tparam Expr
 *   The type of the expressions under analysis.
 */
trait ReturnValue[Expr <: Expression] extends GlobalStore[Expr]:

    def returnAddr(cmp: Component) = ReturnAddr(cmp, expr(cmp).idn)

    // convenience method: final program result = return-addr of the initial component
    def returnValue(cmp: Component): Value = store.getOrElse(returnAddr(cmp), lattice.bottom)
    def finalResult: Value = returnValue(initialComponent)

    // intra-analysis can now also update and read the result of a component
    override def intraAnalysis(cmp: Component): ReturnResultIntra
    trait ReturnResultIntra extends GlobalStoreIntra:
        // updating the result of a component (default: of the current component)
        protected def writeResult(result: Value, cmp: Component = component) = writeAddr(returnAddr(cmp), result)
        // reading the result of a component
        protected def readResult(cmp: Component): Value =
            readAddr(returnAddr(cmp))
        // convenience method: "calling" other components registers a dependency and immediately reads their prior analysis result
        protected def call(cmp: Component): Value =
            spawn(cmp)
            readResult(cmp)
