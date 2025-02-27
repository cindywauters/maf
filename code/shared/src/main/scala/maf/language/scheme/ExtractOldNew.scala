package maf.language.scheme

import maf.core.{Identifier, Identity}
import maf.language.sexp.{SExp, SExpId, SExpPair, SExpValue, Value, SExpTombstone}

import scala.util.control.TailCalls.{done, tailcall}

object ExtractOldNew:

  def buildNew(exp: SExp, idn: Identity): SExp =
    exp match
      case SExpId(Identifier(name, _)) => SExpId(Identifier(name, idn))
      // below case necessary for cases like (<update> 'finished 'done) will not assign the same idn to each other due to the quoted expression
      case SExpPair(SExpId(Identifier("quote", _)), SExpPair(SExpId(Identifier(value, _)), second, _), _) => SExpPair(SExpId(Identifier("quote", idn)), SExpPair(SExpId(Identifier(value, idn)), second, idn), idn)
      case SExpPair(SExpId(Identifier("quote", _)), SExpPair(SExpPair(first, second, _), third, _), _)    => SExpPair(SExpId(Identifier("quote", idn)), SExpPair(SExpPair(first, second, idn), third, idn), idn)
      // Needed for conditionals as the idn given to the desugared if-statement is the idn of the first condition within the conditional 
      case SExpPair(SExpId(Identifier("cond", _)), SExpPair(SExpPair(SExpPair(first, second, _), third, _), fourth, _), _)  => SExpPair(SExpId(Identifier("cond", idn)), SExpPair(SExpPair(SExpPair(first, second, idn), third, idn), fourth, idn), idn)
      case SExpPair(first, second, _) => SExpPair(first, second, idn)
      case SExpValue(value, _) => SExpValue(value, idn)


  def getAllOfVersion(program: SExp, oldversion: Boolean): SExp = program match
    // update: if you want the old version, return the old but with a new idn. Otherwise, return the new
    case SExpPair(SExpId(Identifier("<update>", idn)), SExpPair(old, SExpPair(nw, _, _), _), _) =>
      if oldversion then
        buildNew(old, idn)
      else buildNew(nw, idn)
    case SExpPair(SExpId(Identifier("<change>", idn)), SExpPair(old, SExpPair(nw, _, _), _), _) =>
      if oldversion then
        buildNew(old, idn)
      else
        var nwreturn = buildNew(nw, idn)
        nwreturn
    // If there is an insert and we are looking for the new version: take the SExp otherwise insert placeholder that will be removed later
    case SExpPair(SExpId(Identifier("<insert>", _)), SExpPair(toInsert, _, _), idn) =>
      if oldversion then
        SExpTombstone(idn)
      else toInsert
    // If there is an deletion and we are looking for the old version: take the SExp otherwise insert placeholder that will be removed later
    case SExpPair(SExpId(Identifier("<delete>", _)), SExpPair(toDelete, _, _), idn)=>
      if oldversion then
        toDelete
      else
        SExpTombstone(idn)
    // Pair of other expressions
    case SExpPair(exp1, exp2, idn1) =>
      // first get all old/new expressions of the first expression
      getAllOfVersion(exp1, oldversion) match
        // If it is an expression to remove (like <deletion> when we're looking for the new version)
        case SExpTombstone(_) =>
          // Check the second expression
          getAllOfVersion(exp2, oldversion) match
            // if that is also to be removed, insert a place holder to remove this entire expression (filtered out later)
            case SExpTombstone(_) =>
              SExpTombstone(idn1)
            // Otherwise, the second expression exists (with the right version filtered out) and we return that
            case partial: _ =>
              partial
        // In case the first expression is not to be removed
        case partial1: _ =>
          // Check the second version
          getAllOfVersion(exp2, oldversion) match
            // if that is to be removed: only return the first expression
            case SExpTombstone(_) =>
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
    (old, nw)

