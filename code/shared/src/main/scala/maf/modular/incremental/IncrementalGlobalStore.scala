package maf.modular.incremental

import maf.core.Expression
import maf.language.change.CodeVersion._
import maf.modular._
import maf.util.Annotations._
import maf.util.benchmarks.Timeout
import maf.util.graph.Tarjan

/**
 * This trait improves upon a basic incremental analysis (with dependency and component invalidation) by introducing
 * store "provenance" tracking and store lowering.
 *
 * @tparam Expr The type of the expressions under analysis.
 */
trait IncrementalGlobalStore[Expr <: Expression] extends IncrementalModAnalysis[Expr] with GlobalStore[Expr] {
  inter =>

  /* ****************************************** */
  /* ***** Provenance tracking for values ***** */
  /* ****************************************** */

  /** Keeps track of the provenance of values. For every address, couples every component with the value it has written to the address. */
  var provenance: Map[Addr, Map[Component, Value]] = Map().withDefaultValue(Map().withDefaultValue(lattice.bottom))

  /** Caches the addresses written by every component. Used to find addresses that are no longer written by a component. */
  var cachedWrites: Map[Component, Set[Addr]] = Map().withDefaultValue(Set())

  /** Computes the value that should reside at a given address according to the provenance information. */
  def provenanceValue(addr: Addr): Value = provenance(addr).values.fold(lattice.bottom)(lattice.join(_, _))

  /** Updates the provenance information for a specific component and address. */
  def updateProvenance(
      cmp: Component,
      addr: Addr,
      value: Value
    ): Unit =
    provenance = provenance + (addr -> (provenance(addr) + (cmp -> value)))

  /* ******************************************** */
  /* ***** Managing cyclic value provenance ***** */
  /* ******************************************** */

  /**
   * Approximates the value flow of the analysis. For every component, stores a map of W ~> Set[R], if W was the most recent write after reading R.
   *
   * @note The data is separated by components, so it can be reset upon the reanalysis of a component.
   * @note We could also store it as R ~> Set[W], but this seems slightly easier (doesn't require a foreach over the set `reads`).
   *       Also, there might be multiple R per W, but having multipel W per R seems more rare (though it can happen when there are branches in the code of course)
   */
  var addressDependencies: Map[Component, Map[Addr, Set[Addr]]] = Map().withDefaultValue(Map().withDefaultValue(Set()))

  /**
   * Keeps track of all inferred SCCs during an incremental update.
   * To avoid confusion with analysis components, we call these Strongly Connected Addresses (SCC containing addresses).
   */
  var SCAs: Set[Set[Addr]] = Set()

  /* ****************************** */
  /* ***** Write invalidation ***** */
  /* ****************************** */

  /**
   * To be called when a write dependency is deleted. Possibly updates the store with a new value for the given address.
   * An address that is no longer written will be set to bottom.
   *
   * @param cmp  The component from which the write dependency is deleted.
   * @param addr The address corresponding to the deleted write dependency.
   */
  @nonMonotonicUpdate
  def deleteProvenance(cmp: Component, addr: Addr): Unit = {
    // Delete the provenance information corresponding to this component.
    provenance = provenance + (addr -> (provenance(addr) - cmp))
    // Compute the new value for the address and update it in the store.
    val value: Value = provenanceValue(addr)
    if (value != inter.store(addr)) {
      trigger(???, AddrDependency(addr)) // Trigger first, as the dependencies may be removed should the address be deleted.
      // Small memory optimisation: clean up addresses entirely when they become not written anymore. This will also cause return addresses to be removed upon component deletion.
      if (provenance(addr).isEmpty)
        deleteAddress(addr)
      else inter.store = inter.store + (addr -> value)
    }
  }

  /**
   * Deletes an address from the store. To be used when they are no longer written by any component.
   *
   * @note Also removes possible dependencies on this address!
   */
  def deleteAddress(addr: Addr): Unit = {
    store -= addr // Delete the address in the actual store.
    provenance -= addr // Remove provenance information corresponding to the address (to ensure the right dependencies are triggered should the address be recreated and obtain the same value).
    deps -= AddrDependency(addr) // Given that the address is no longer in existence, dependencies on this address can be removed.
  }

  /* ********************************** */
  /* ***** Component invalidation ***** */
  /* ********************************** */

  /**
   * Called when a component is deleted. Removes the provenance information corresponding to the addresses written by the
   * given component, thereby possibly refining the analysis store.
   *
   * @param cmp The component that is deleted.
   */
  override def deleteComponent(cmp: Component): Unit = {
    cachedWrites(cmp).foreach(deleteProvenance(cmp, _))
    cachedWrites = cachedWrites - cmp
    super.deleteComponent(cmp)
  }

  /* *************************************************** */
  /* ***** Incremental value update and refinement ***** */
  /* *************************************************** */

  /**
   * To be called upon a commit, with the join of the values written by the component to the given address.
   * Possibly updates the store and provenance information, and triggers dependencies if the store is updated.
   *
   * @param cmp  The component that is committed.
   * @param addr The address in the store of value nw.
   * @param nw   The join of all values written by cmp to the store at addr.
   * @return Returns a boolean indicating whether the address was updated,
   *         and hence whether the corresponding dependency should be triggered.
   */
  @nonMonotonicUpdate
  def updateAddrInc(
      cmp: Component,
      addr: Addr,
      nw: Value
    ): Boolean = {
    val old = provenance(addr)(cmp)
    if (old == nw) return false // Nothing changed.
    // Else, there is some change. Note that both `old ⊏ nw` and `nw ⊏ old` - or neither - are possible.
    updateProvenance(cmp, addr, nw)
    val oldJoin = inter.store.getOrElse(addr, lattice.bottom) // The value currently at the given address.
    // If `old ⊏ nw` we can just use join, which is probably more efficient.
    val newJoin = if (lattice.subsumes(nw, old)) lattice.join(oldJoin, nw) else provenanceValue(addr)
    if (oldJoin == newJoin) return false // Even with this component writing a different value to addr, the store does not change.
    inter.store = inter.store + (addr -> newJoin)
    true
  }

  /* ************************************************************************* */
  /* ***** Incremental update: actually perform the incremental analysis ***** */
  /* ************************************************************************* */

  var tarjanFlag: Boolean = optimisationFlag // Flag to enable the latest optimization.

  override def updateAnalysis(timeout: Timeout.T, optimisedExecution: Boolean): Unit = {
    if (tarjanFlag)
      SCAs = Tarjan.scc[Addr](store.keySet, addressDependencies.values.flatten.groupBy(_._1).map({ case (w, wr) => (w, wr.flatMap(_._2).toSet) }))
    super.updateAnalysis(timeout, optimisedExecution)
    if (tarjanFlag) SCAs = Set()
  }

  /* ************************************ */
  /* ***** Intra-component analysis ***** */
  /* ************************************ */

  trait IncrementalGlobalStoreIntraAnalysis extends IncrementalIntraAnalysis with GlobalStoreIntra {
    intra =>

    abstract override def analyzeWithTimeout(timeout: Timeout.T): Unit = {
      if (tarjanFlag) addressDependencies = addressDependencies - component // Avoid data becoming wrong/outdated after an incremental update.
      super.analyzeWithTimeout(timeout)
    }

    var reads: Set[Addr] = Set()

    override def readAddr(addr: Addr): Value = {
      if (tarjanFlag) {
        reads += addr
        if (version == New) {
          // Zero or one SCCs will be found.
          SCAs.find(_.contains(addr)).foreach { sca =>
            // TODO: should only this be triggered, or should every address in the SCC be triggered? (probably one suffices)
            trigger(AddrDependency(addr))
            // Should be done for every address in the SCA because an SCC/SCA may contain "inner cycles".
            sca.foreach { addr =>
              inter.store += addr -> lattice.bottom // Todo, but value of the entrance of the cycle??????
              intra.store += addr -> lattice.bottom
              provenance -= addr
              updateProvenance(component,
                               addr,
                               lattice.bottom
              ) // Avoid spurious deletions?! TODO investigate: leads to double the no of assertions in some situations...
            }
            SCAs -= sca
          }
        }
      }
      // TODO: if bottom is read, will the analysis continue appropriately?
      super.readAddr(addr)
    }

    /**
     * Keep track of the values written by a component to an address.
     * For every address, stores the join of all values written during this intra-component address.
     */
    // TODO: Perhaps collapse this data structure in the global provenance information (requires updating the information without triggers etc, but with joining).
    var intraProvenance: Map[Addr, Value] = Map().withDefaultValue(lattice.bottom)

    override def writeAddr(addr: Addr, value: Value): Boolean = {
      // Update the intra-provenance: for every address, keep the join of the values written to the address.
      intraProvenance = intraProvenance + (addr -> lattice.join(intraProvenance(addr), value))
      // Update the value flow information and reset the reads information.
      if (tarjanFlag) {
        addressDependencies =
          addressDependencies + (component -> (addressDependencies(component) + (addr -> (addressDependencies(component)(addr) ++ reads))))
        reads = Set()
      }
      // Ensure the intra-store is updated so it can be used. TODO should updateAddrInc be used here (but working on the intra-store) for an improved precision?
      super.writeAddr(addr, value)
    }

    /**
     * Called for every written address. Returns true if the dependency needs to be triggered.
     *
     * @note This function should be overridden to avoid the functionality of GlobalStore to be used.
     *       Otherwise this function could be merged into refineWrites.
     */
    override def doWrite(dep: Dependency): Boolean = dep match {
      case AddrDependency(addr) if optimisationFlag =>
        // There is no need to use the updateAddr function, as the store is updated by updateAddrInc.
        // Also, this would not work, as updateAddr only performs monotonic updates.
        updateAddrInc(component, addr, intraProvenance(addr))
      case _ => super.doWrite(dep)
    }

    /**
     * Registers the provenance information to the global provenance registry.
     *
     * @note Will also update the provenances that were already updated by updateAddrInc (but this is ok as the same value is used).
     */
    def registerProvenances(): Unit = intraProvenance.foreach({ case (addr, value) => updateProvenance(component, addr, value) })

    /** Refines values in the store that are no longer written to by a component. */
    @nonMonotonicUpdate
    def refineWrites(): Unit = {
      // Writes performed during this intra-component analysis. Important: this only works when the entire component is reanalysed!
      val recentWrites = intraProvenance.keySet
      if (version == New) {
        // The addresses previously written to by this component, but that are no longer written by this component.
        val deltaW = cachedWrites(component) -- recentWrites
        deltaW.foreach(deleteProvenance(component, _))
      }
      cachedWrites = cachedWrites + (component -> recentWrites)
    }

    /** First performs the commit. Then uses information inferred during the analysis of the component to refine the store if possible. */
    override def commit(): Unit = {
      super.commit() // First do the super commit, as this will cause the actual global store to be updated.
      if (optimisationFlag) {
        refineWrites() // Refine the store by removing extra addresses or using the provenance information to refine the values in the store.
        registerProvenances() // Make sure all provenance values are correctly stored, even if no doWrite is triggered for the corresponding address.
      }
    }
  }

}
