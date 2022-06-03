package maf.language.CScheme

import maf.core.{Expression, Identifier}
import maf.core.Position.*
import maf.language.scheme.*
import maf.language.sexp.*
import maf.language.scheme.primitives.SchemePrelude

object CSchemeParserWithSplitter:
  /** Replace defines in a program (a list of expressions) by a big letrec as a single expression. */
  def undefine(exps: List[SchemeExp]): SchemeExp = CSchemeUndefiner.undefine(exps)

  /** Parse a string representing a CScheme program. */
  def parse(s: String, tag: PTag = noTag): (List[SchemeExp], List[SchemeExp]) =
    val bothVersions = SExpParser.parse(s, tag).map(ExtractOldNew.getOldNewVersions)
    //println(bothVersions)
    val oldVersion = bothVersions.map(_._1).filter(e => e match
      case SExpTombstone(_) => false
      case _ => true).map(CSchemeParser.compile)
    val newVersion = bothVersions.map(_._2).filter(e => e match
      case SExpTombstone(_) => false
      case _ => true).map(CSchemeParser.compile)
    (oldVersion, newVersion)

  /** Parse a program, add its prelude and undefine it */
  def parseProgram(prg: String, tag: PTag = noTag): (SchemeExp, SchemeExp) =
    val parsed = parse(prg, tag)
    val preludeoneFull = SchemePrelude.addPrelude(parsed._1)
    val preludetwoFull = SchemePrelude.addPrelude(parsed._2)
    val preludeOne = preludeoneFull.take(preludeoneFull.size - parsed._1.size).toSet
    val preludeTwo = preludetwoFull.take(preludetwoFull.size - parsed._2.size).toSet
    val prelude = (preludeOne ++ preludeTwo).toList
    (undefine(prelude.appendedAll(parsed._1)), undefine(prelude.appendedAll(parsed._2)))

