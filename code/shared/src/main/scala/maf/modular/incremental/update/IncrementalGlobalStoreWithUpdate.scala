package maf.modular.incremental.update

import maf.core.Expression
import maf.modular.incremental.IncrementalGlobalStore
import maf.util.benchmarks.Timeout

trait IncrementalGlobalStoreWithUpdate[Expr <: Expression] extends IncrementalModAnalysisWithUpdate[Expr] with IncrementalGlobalStore[Expr]:

    override def updateAnalysis(timeout: Timeout.T): Unit =
        if configuration.cyclicValueInvalidation then
            val SCAs = computeSCAs()
            val addrs = SCAs.flatten
            addrs.flatMap(provenance).map(_._1).foreach(addToWorkList)
            addrs.foreach { addr =>
                store = store + (addr -> lattice.bottom)
                provenance -= addr
            }
            cachedWrites = cachedWrites.map({ case (k, v) => (k, v -- addrs) }).withDefaultValue(Set())
        super.updateAnalysis(timeout)