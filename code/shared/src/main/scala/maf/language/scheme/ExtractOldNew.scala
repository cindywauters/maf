package maf.language.scheme

import maf.core.Identifier
import maf.language.sexp.{SExp, SExpId, SExpPair, SExpValue, Value}

import scala.util.control.TailCalls.{done, tailcall}

object ExtractOldNew:

  def getAllOfVersion(program: SExp, oldversion: Boolean): SExp = program match
    case SExpPair(SExpId(Identifier("<update>", idn)), SExpPair(SExpId(Identifier(oldId, _)), SExpPair(SExpId(Identifier(newId, _)), _, _), _), _) if oldversion =>
      SExpId(Identifier(oldId, idn))
    case SExpPair(SExpId(Identifier("<update>", idn)), SExpPair(SExpId(Identifier(oldId, _)), SExpPair(SExpId(Identifier(newId, _)), _, _), _), _) if !oldversion =>
      SExpId(Identifier(newId, idn))
    // If there is an insert and we are looking for the new version: take the SExp
    case SExpPair(SExpId(Identifier("<insert>", _)), SExpPair(toInsert, _, _), _) if !oldversion =>
      toInsert
    // Otherwise, put a "to remove sexp" placeholder (filtered out later)
    case SExpPair(SExpId(Identifier("<insert>", _)), SExpPair(toInsert, _, idn), _) if oldversion =>
      SExpPair(SExpId(Identifier("to remove sexp", idn)), SExpValue(Value.Nil, idn), idn)
    // If there is an deletion and we are looking for the old version: take the SExp
    case SExpPair(SExpId(Identifier("<delete>", _)), SExpPair(toDelete, _, _), _) if oldversion =>
      toDelete
    // Otherwise, put a "to remove sexp" placeholder (filtered out later)
    case SExpPair(SExpId(Identifier("<delete>", _)), SExpPair(toDelete, _, idn), _) if !oldversion =>
      SExpPair(SExpId(Identifier("to remove sexp", idn)), SExpValue(Value.Nil, idn), idn)
    // Change expression and we want the old version: take the first expression
    case SExpPair(SExpId(Identifier("<change>", _)), SExpPair(old, SExpPair(nw, _, _), _), _) if oldversion =>
      old
    // Change expression and we want the new version: take the second expression
    case SExpPair(SExpId(Identifier("<change>", _)), SExpPair(old, SExpPair(nw, _, _), _), _) if !oldversion =>
      nw
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


 //   case SExpPair(SExpId(Identifier("<insert>", _)), SExpPair(expr, SExpValue(Value.Nil, _), _), _) =>
/*  tailcall(this._compile(expr)).map(CSchemeFork(_, exp.idn))
  case SExpPair(SExpId(Identifier("fork", _)), _, _) =>
  throw new Exception(s"Invalid CScheme fork: $exp (${exp.idn}).")
  case SExpPair(SExpId(Identifier("join", _)), SExpPair(expr, SExpValue(Value.Nil, _), _), _) =>
  tailcall(this._compile(expr)).map(CSchemeJoin(_, exp.idn))
  case SExpPair(SExpId(Identifier("join", _)), _, _) =>
  throw new Exception(s"Invalid CScheme join: $exp (${exp.idn}).")
    println(program)
    (program, program)*/
