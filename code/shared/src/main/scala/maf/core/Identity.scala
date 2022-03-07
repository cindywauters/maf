package maf.core

import maf.core.Identity._
import maf.core.Position._
import maf.util.SmartHash

/** An identity to distinguish expressions. */
sealed trait Identity extends Serializable:
    val idn: IDN
    def pos: Position = idn // Extra positional information of the element in the source code. Used for printing and during tests.
    override def toString: String = pos.toString

/** An identity for AST elements. */
case class SimpleIdentity(idn: IDN) extends Identity with SmartHash

/** Neutral identity for to elements not in the code (constructed by the analysis). */
case object NoCodeIdentity extends Identity:
    val idn: IDN = Identity.newId(Position(-1, 0))
case object NoCodeIdentityDebug extends Identity:
    val idn: IDN = Identity.newId(Position(-2, 0))

/*
trait UniqueIdentity {
  type IDN = Long // Type name is IDN to avoid confusion with identifiers.

  implicit def toPosition(idn: IDN): Position = iMap(idn)

  implicit val identityOrdering: Ordering[Identity] = new Ordering[Identity] {
    def compare(x: Identity, y: Identity): Int = (x,y) match {
      case (NoCodeIdentity, NoCodeIdentity) => 0
      case (NoCodeIdentity, _) => -1
      case (_, NoCodeIdentity) => 1
      case (s1: SimpleIdentity, s2: SimpleIdentity) =>
        Ordering.by[SimpleIdentity,Long](_.idn).compare(s1,s2)
    }
  }

  /** Contains positional information for identifiers. ALL ACCESSES TO iMap HAVE TO BE SYNCHRONISED ON THE Identity OBJECT. */
  var iMap: Map[IDN, Position] = Map()

  /** Contains the last unused identity. ALL ACCESSES TO ctr HAVE TO BE SYNCHRONISED ON THE Identity OBJECT. */
  private var ctr: Long = 0
  def newId(pos: Position): IDN = Identity.synchronized {
    ctr = ctr + 1
    ctr - 1
  }

  def apply(p: scala.util.parsing.input.Position, t: PTag = noTag): Identity = Identity.synchronized {
    val pos: Position = Position(p.line, p.column, t)
    val idn: IDN = newId(pos)
    iMap = iMap + (idn -> pos)
    SimpleIdentity(idn)
  }
}
 */

trait PositionalIdentity:
    type IDN = Position
    def apply(p: scala.util.parsing.input.Position, t: PTag = noTag): Identity = SimpleIdentity(Position(p.line, p.column, t))
    def newId(pos: Position): IDN = pos

// To go back to the other identity implementation, comment out PositionalIdentity, reinstate UniqueIdentity and change the inheritance below.
object Identity extends PositionalIdentity:
    def none: Identity = NoCodeIdentity

object Position:
    // Tag for positions (can e.g. be used when ASTs of multiple parsings need to be combined).
    sealed trait PTag:
        /** Geneates a showable representation of the tag */
        def show: String

        /** Checks wheter the tag contains the given string */
        def contains(v: String): Boolean

        /** Returns true if the position does not have a tag */
        def isEmpty: Boolean = false

    case class SimplePTag(tag: String) extends PTag:
        def show: String = tag
        def contains(v: String): Boolean = tag.contains(v)

    case class PTagWithSource(tag: String, path: String) extends PTag:
        def show: String = s"$tag:$path"
        def contains(v: String): Boolean = tag.contains(v)

    case class SourcePathTag(sourcePath: String) extends PTag:
        def show: String = s"$sourcePath"
        def contains(v: String): Boolean = false
        override def isEmpty: Boolean = true

    case object NoPTag extends PTag:
        override def isEmpty: Boolean = true
        def show: String = ""
        def contains(v: String): Boolean = false

    val noTag: PTag = NoPTag
    def newTag(tag: String): PTag = SimplePTag(tag)
    def newTag(tag: String, path: String): PTag = PTagWithSource(tag, path)
    def withSourcePath(path: String) = SourcePathTag(path)

    /**
     * Positional information of an expression.
     *
     * @param line
     *   the line on which the expression occurs in the source code
     * @param col
     *   the column on which the expression occurs in the source code
     * @param tag
     *   an optional tag, can be used to differentiate the AST from multiple parsings
     */
    case class Position(
        line: Int,
        col: Int,
        tag: PTag = noTag)
        extends SmartHash:
        override def toString: String = tag match
            case NoPTag => s"$line:$col"
            case t      => s"${t.show}:$line:$col"

    def apply(
        line: Int,
        col: Int,
        tag: PTag = noTag
      ): Position = Position(line, col, tag)
