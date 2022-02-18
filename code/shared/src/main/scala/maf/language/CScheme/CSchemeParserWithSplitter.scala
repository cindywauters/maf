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
    val oldVersion = bothVersions.map(_._1).filter(e => e match
      case SExpPair(SExpId(Identifier("to remove sexp", _)), SExpValue(Value.Nil, _), _) => false
      case _ => true).map(CSchemeParser.compile)
    val newVersion = bothVersions.map(_._2).filter(e => e match
      case SExpPair(SExpId(Identifier("to remove sexp", _)), SExpValue(Value.Nil, _), _) => false
      case _ => true).map(CSchemeParser.compile)

    (oldVersion, newVersion)

  def findAllSubExps(expr: Expression): List[Expression] =
    if  expr.subexpressions.isEmpty then
      List(expr)
    else expr match
      case _ => List(expr).appendedAll(expr.subexpressions.flatMap(e => findAllSubExps(e)))

  /** Parse a program, add its prelude and undefine it */
  def parseProgram(prg: String, tag: PTag = noTag): (SchemeExp, SchemeExp) =
    val parsed = parse(prg, tag)
    println(parsed._1)
   // parsed._1.foreach(e => e.subexpressions.foreach(e => print(e.idn.toString + " ")))
  //  println()
    parsed._1.foreach(e => findAllSubExps(e).foreach(e => println(e.toString + " " + e.idn.toString  + " ")))
    parsed._1.foreach(e => findAllSubExps(e).foreach(e => print(e.idn.toString + " ")))
    println()
    println(parsed._2)
    parsed._2.foreach(e => findAllSubExps(e).foreach(e => print(e.idn.toString + " ")))
  //  parsed._2.foreach(e => e.subexpressions.foreach(e => print(e.idn.toString + " ")))
    println()
    parsed._2.foreach(e => e.subexpressions.foreach(e => println(e.idn.toString + " " + e.toString)))
    (undefine(SchemePrelude.addPrelude(parsed._1)), undefine(SchemePrelude.addPrelude(parsed._2)))

