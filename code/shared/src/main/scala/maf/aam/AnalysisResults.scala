package maf.aam

import maf.core.*

trait AnalysisResults extends maf.aam.AAMAnalysis:
    def resultsPerIdn: Map[Identity, Set[Val]]

trait SchemeAAMAnalysisResults extends SchemeAAMSemantics with maf.aam.AnalysisResults:
    var resultsPerIdn = Map().withDefaultValue(Set.empty[Val])

    override def writeSto(sto: Sto, addr: Address, value: Storable): Sto =
        (addr, value) match
            case (_: VarAddr, Storable.V(value)) =>
              resultsPerIdn += addr.idn -> (resultsPerIdn(addr.idn) + value)
            case _ => ()
        super.writeSto(sto, addr, value)
