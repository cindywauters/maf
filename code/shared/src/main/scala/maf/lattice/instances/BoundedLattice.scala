package maf.lattice.instances

class BoundedLattice(val k: Int) extends ConcreteLattice {
  override def makeValues[X](contents: Set[X]): L[X] =
    if (contents.size <= k) {
      Values(contents)
    } else {
      Top
    }
}
