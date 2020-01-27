package scalaam.modular

import scalaam.core._
import scalaam.util.Annotations._
import scalaam.util.MonoidImplicits._

/**
 * Adds a global store to the analysis. This store supports various addressing modes.
 * @tparam Expr The type of the expressions under analysis.
 */
trait GlobalStore[Expr <: Expression] extends ModAnalysis[Expr] {

  // parameterized by a type that represents (local) addresses
  type LocalAddr <: Address
  // parameterized by a type that represents abstract values
  type Value
  implicit val lattice: Lattice[Value]

  // addresses in the global analysis are (local) addresses of the intra-analysis + the component
  trait Addr extends Address
  case class ComponentAddr(cmp: Component, addr: LocalAddr) extends Addr {
    def printable: Boolean = addr.printable
  }

  // the global store of the analysis
  @mutable var store: Map[Addr,Value] = Map()
  private def updateAddr(addr: Addr, value: Value): Boolean = store.get(addr) match {
    case None if value == lattice.bottom => false
    case None => store = store + (addr -> value); true
    case Some(oldValue) =>
      val newValue = lattice.join(oldValue,value)
      if (newValue == oldValue) return false
      store = store + (addr -> newValue)
      true
  }

  // Dependency that is triggered when an abstract value at address 'addr' is updated
  case class ReadWriteDependency(addr: Addr) extends Dependency

  trait GlobalStoreIntra extends super.IntraAnalysis {

    // allocating an address
    def allocAddr(addr: LocalAddr): ComponentAddr =
      ComponentAddr(component, addr)

    // reading addresses in the global store
    protected def readAddr(addr: LocalAddr, cmp: Component = component): Value =
      readAddr(ComponentAddr(cmp, addr))
    protected def readAddr(addr: Addr): Value = {
      registerDependency(ReadWriteDependency(addr))
      store.get(addr) match {
        case None => store += (addr -> lattice.bottom) ; lattice.bottom
        case Some(v) => v
      }
    }

    // writing addresses of the global store
    protected def writeAddr(addr: LocalAddr, value: Value, cmp: Component = component): Unit =
        writeAddr(ComponentAddr(cmp,addr),value)
    protected def writeAddr(addr: Addr, value: Value): Unit =
        if (updateAddr(addr,value)) // If the value in the store changed, trigger the dependency.
          triggerDependency(ReadWriteDependency(addr))
    }
}

trait AdaptiveGlobalStore[Expr <: Expression] extends AdaptiveModAnalysis[Expr] with GlobalStore[Expr] {
  // alpha definition for addresses and dependencies
  def alphaAddr(addr: Addr): Addr = addr match {
    case ComponentAddr(cmp,localAddr) => ComponentAddr(alpha(cmp),localAddr)
  }
  override def alphaDep(dep: Dependency): Dependency = dep match {
    case ReadWriteDependency(addr) => ReadWriteDependency(alphaAddr(addr))
    case _ => super.alphaDep(dep)
  }
  // requires an implementation of alpha for the abstract domain
  def alphaValue(value: Value): Value
  // when abstraction map changes, need to update the store
  override def onAlphaChange(): Unit = {
    super.onAlphaChange()
    val oldStore = store
    store = alphaMap(alphaAddr,alphaValue)(store)
    // look if we have to retrigger any dependencies due to the abstraction
    oldStore.foreach { case (oldKey, oldValue) =>
      val newKey = alphaAddr(oldKey)
      val newValue = store(newKey)
      if (oldValue != newValue) {
        triggerDependency(ReadWriteDependency(newKey))
      }
    }
  }
}
