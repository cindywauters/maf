package maf.test.language.scheme

import org.scalacheck.Prop._
import maf.core._
import maf.language.scheme.lattices._
import maf.test.lattice._

// inherits the standard lattice tests from `LatticeTest`
class SchemeLatticeTests[L](gen: SchemeLatticeGenerator[L])(implicit val schemeLattice: SchemeLattice[L, _, _]) extends LatticeTest(gen) {
  // because of higher entropy in Scheme lattice values, verify each property with more examples!
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)
  val schemeLaws = newProperties("Scheme") { p =>
    implicit val arb = gen.anyArb
    implicit val shr = gen.shrink
    implicit val auo = gen.arbUnop
    implicit val abo = gen.arbBinop
    implicit val ato = gen.arbTernop

    import schemeLattice._
    /* */
    def convert(mf: MayFail[L, Error]): L = mf match {
      case MayFailSuccess(v) => v
      case MayFailBoth(v, _) => v
      case MayFailError(_)   => bottom
    }
    /* Unary operators preserve bottom */
    p.property("∀ unop: unop(⊥) = ⊥") = forAll((unop: SchemeOp.SchemeOp1) => convert(op(unop)(List(bottom))) == bottom)
    /* Unary operators are monotone */
    /* TODO: fails because of NaN
        p.property("∀ unop, a, b: a ⊑ b ⇒ unop(a) ⊑ unop(b)") = forAll { (unop: SchemeOps.UnaryOperator, b: L) =>
            forAll(gen.le(b)) { (a: L) =>
                val fa = convert(unaryOp(unop)(a))
                val fb = convert(unaryOp(unop)(b))
                println(s"f = $unop")
                println(s"f($a) = $fa")
                println(s"f($b) = $fb")
                subsumes(b, a) ==> subsumes(fb, fa)
            }
        }
     */
    /* Binary operators preverse bottom */
    p.property("∀ binop, a: binop(⊥,a) = ⊥ = binop(a,⊥)") =
      forAll((binop: SchemeOp.SchemeOp2, a: L) =>
        convert(op(binop)(List(bottom, a))) == bottom &&
          convert(op(binop)(List(a, bottom))) == bottom)
    /* Ternary operators preserve bottom */
    p.property("∀ ternop, a: ternop(⊥,a,b) = ⊥ = ternop(a,⊥,b) = ternop(a,b,⊥)") =
      forAll((ternop: SchemeOp.SchemeOp3, a: L, b: L) =>
        convert(op(ternop)(List(bottom, a, b))) == bottom &&
          convert(op(ternop)(List(a, bottom, b))) == bottom &&
            convert(op(ternop)(List(a, b, bottom))) == bottom)

    /* Properties about vectors */
    p.property("∀ vct, idx1, val1, idx2, val2: vectorSet(vectorSet(vct,idx1,val1),idx2,val2) = vectorSet(vectorSet(vct,idx2,val2),idx1,val1)") =
      forAll(gen.anyVec, gen.anyInt, gen.any, gen.anyInt, gen.any) { (vct: L, idx1: L, val1: L, idx2: L, val2: L) =>
        val vct1 = convert(vectorSet(vct, idx1, val1))
        val vct2 = convert(vectorSet(vct, idx2, val2))
        val vct12 = convert(vectorSet(vct1, idx2, val2))
        val vct21 = convert(vectorSet(vct2, idx1, val1))
        s"vectorSet(vct,$idx1,$val1) = $vct1" |:
          s"vectorSet(vct,$idx2,$val2) = $vct2" |:
          s"vectorSet(vectorSet(vct,$idx1,$val1),$idx2,$val2) = $vct12" |:
          s"vectorSet(vectorSet(vct,$idx2,$val2),$idx1,$val1) = $vct21" |:
          vct12 == vct21
      }
    // return the properties
    p
  }
  checkAll(schemeLaws)
}

class ConcreteSchemeLatticeTests extends SchemeLatticeTests(ConcreteModularSchemeLattice.SchemeValueLatticeGenerator)
class ConstantSchemeLatticeTests extends SchemeLatticeTests(ConstantModularSchemeLattice.SchemeValueLatticeGenerator)
class TypeSchemeLatticeTests extends SchemeLatticeTests(TypeModularSchemeLattice.SchemeValueLatticeGenerator)
