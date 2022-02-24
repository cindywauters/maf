package maf.language.scheme

import maf.core.{Identifier, Identity}
import maf.language.sexp.{SExp, SExpId, SExpPair, SExpValue, Value}

import scala.util.control.TailCalls.{done, tailcall}

object ExtractOldNew:

  def buildNew(exp: SExp, idn: Identity): SExp =
    exp match
      case SExpId(Identifier(name, _)) => SExpId(Identifier(name, idn))
      case SExpPair(first, second, _) => SExpPair(first, second, idn)
      case SExpValue(value, _) => SExpValue(value, idn)


  def getAllOfVersion(program: SExp, oldversion: Boolean): SExp = program match
    // update: if you want the old version, return the old but with a new idn. Otherwise, return the new
    case SExpPair(SExpId(Identifier("<update>", idn)), SExpPair(old, SExpPair(nw, _, _), _), _) =>
      if oldversion then
        buildNew(old, idn)
      else buildNew(nw, idn)
    // If there is an insert and we are looking for the new version: take the SExp otherwise insert placeholder that will be removed later
    case SExpPair(SExpId(Identifier("<insert>", _)), SExpPair(toInsert, _, _), idn) =>
      if oldversion then
        SExpPair(SExpId(Identifier("to remove sexp", idn)), SExpValue(Value.Nil, idn), idn)
      else toInsert
    // If there is an deletion and we are looking for the old version: take the SExp otherwise insert placeholder that will be removed later
    case SExpPair(SExpId(Identifier("<delete>", _)), SExpPair(toDelete, _, _), idn)=>
      if oldversion then
        toDelete
      else SExpPair(SExpId(Identifier("to remove sexp", idn)), SExpValue(Value.Nil, idn), idn)
    // Pair of other expressions
    case SExpPair(exp1, exp2, idn1) =>
      // first get all old/new expressions of the first expression
      getAllOfVersion(exp1, oldversion) match
        // If it is an expression to remove (like <deletion> when we're looking for the new version)
        case SExpPair(SExpId(Identifier("to remove sexp", _)), SExpValue(Value.Nil, _), _) =>
          // Check the second expression
          getAllOfVersion(exp2, oldversion) match
            // if that is also to be removed, insert a place holder to remove this entire expression (filtered out later)
            case SExpPair(SExpId(Identifier("to remove sexp", _)), SExpValue(Value.Nil, _), _) =>
              SExpPair(SExpId(Identifier("to remove sexp", idn1)), SExpValue(Value.Nil, idn1), idn1)
            // Otherwise, the second expression exists (with the right version filtered out) and we return that
            case partial: _ =>
              partial
        // In case the first expression is not to be removed
        case partial1: _ =>
          // Check the second version
          getAllOfVersion(exp2, oldversion) match
            // if that is to be removed: only return the first expression
            case SExpPair(SExpId(Identifier("to remove sexp", _)), SExpValue(Value.Nil, _), _) =>
              partial1
            // otherwise both shouldn't be removed and they're put back into a pair
            case partial2: _ =>
              SExpPair(partial1, partial2, idn1)
    case SExpId(v) =>
      SExpId(v)
    case SExpValue(value, idn1) =>
      SExpValue(value, idn1)

  def getOldNewVersions(program: SExp): (SExp, SExp) =
    val old = getAllOfVersion(program, true)
    val nw = getAllOfVersion(program, false)
    println("all old:")
    println(old)
    println("all new:")
    println(nw)
    (old, nw)

