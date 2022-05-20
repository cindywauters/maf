package maf.modular.incremental.update

import maf.core.{Identifier, NoCodeIdentity}
import maf.language.scheme.*

/**
 * almost entirely the same as SchemeRenamer with the only difference being in the count1 function Needed to find consistent renamings
 */
object SchemeChangeRenamerForPatterns:

    /** Maps each variables to their alpha-renamed version (eg. x -> _x0) */
    type NameMap = Map[String, String]

    /** Map each variables to the number of times it is bound */
    type CountMap = Map[String, Int]

    def rename(exp: SchemeExp): SchemeExp =
        rename(exp, Map[String, String](), Map[String, Int]()) match
            case (e, _) => e

    def rename(
                  exp: SchemeExp,
                  names: NameMap,
                  count: CountMap
              ): (SchemeExp, CountMap) = exp match
        case SchemeLambda(name, args, body, annotation, pos) =>
            countl(args, names, count) match
                case (args1, names1, count1) =>
                    renameList(body, names1, count1) match
                        case (body1, count2) => (SchemeLambda(name.map(n => names.getOrElse(n, n)), args1, body1, annotation, pos), count2)
        case SchemeFuncall(f, args, pos) =>
            rename(f, names, count) match
                case (f1, count1) =>
                    renameList(args, names, count1) match
                        case (args1, count2) => (SchemeFuncall(f1, args1, pos), count2)
        case SchemeIf(cond, cons, alt, pos) =>
            rename(cond, names, count) match
                case (cond1, count1) =>
                    rename(cons, names, count1) match
                        case (cons1, count2) =>
                            rename(alt, names, count2) match
                                case (alt1, count3) => (SchemeIf(cond1, cons1, alt1, pos), count3)
        case SchemeLet(bindings, body, pos) =>
            countl(bindings.map(_._1), names, count) match
                /* Use old names for expressions of bindings */
                case (variables, names1, count1) =>
                    renameList(bindings.map(_._2), names, count1) match
                        case (exps, count2) =>
                            renameList(body, names1, count2) match
                                case (body1, count3) => (SchemeLet(variables.zip(exps), body1, pos), count3)
        case SchemeLetStar(bindings, body, pos) =>
            renameLetStarBindings(bindings, names, count) match
                case (bindings1, names1, count1) =>
                    renameList(body, names1, count1) match
                        case (body1, count2) => (SchemeLetStar(bindings1, body1, pos), count2)
        case SchemeLetrec(bindings, body, pos) =>
            countl(bindings.map(_._1), names, count) match
                /* Use new names for expressions of bindings */
                case (variables, names1, count1) =>
                    renameList(bindings.map(_._2), names1, count1) match
                        case (exps, count2) =>
                            renameList(body, names1, count2) match
                                case (body1, count3) => (SchemeLetrec(variables.zip(exps), body1, pos), count3)

        case SchemeSet(variable, value, pos) =>
            rename(value, names, count) match
                case (value1, count1) =>
                    (SchemeSet(names.get(variable.name) match {
                        case Some(n) => Identifier(n, variable.idn)
                        case None    => variable
                    },
                        value1,
                        pos
                    ),
                        count1
                    )
        case SchemeBegin(body, pos) =>
            renameList(body, names, count) match
                case (body1, count1) => (SchemeBegin(body1, pos), count1)
        case SchemeAssert(exp, pos) =>
            rename(exp, names, count) match
                case (exp1, count1) => (SchemeAssert(exp1, pos), count1)
        case SchemeDefineVariable(name, value, pos) =>
            /* Keeps name untouched (maybe not correct?) */
            rename(value, names, count) match
                case (value1, count1) => (SchemeDefineVariable(name, value1, pos), count1)
        case SchemeVar(id) =>
            names.get(id.name) match
                case Some(n) => (SchemeVar(Identifier(n, id.idn)), count)
                case None    => (SchemeVar(Identifier(id.name, id.idn)), count) /* keep original name */
        case SchemeValue(v, pos) =>
            (SchemeValue(v, pos), count)
        case _ => throw new Exception(s"Unhandled expression in renamer: $exp")

    /** Renames a list of expressions executed sequentially (eg. within a begin) */
    def renameList(
                      exps: List[SchemeExp],
                      names: NameMap,
                      count: CountMap
                  ): (List[SchemeExp], CountMap) = exps match
        case exp :: rest =>
            val (exp1, count1) = rename(exp, names, count)
            val (rest1, count2) = renameList(rest, names, count1)
            (exp1 :: rest1, count2)
        case Nil => (Nil, count)

    def renameLetStarBindings(
                                 bindings: List[(Identifier, SchemeExp)],
                                 names: NameMap,
                                 count: CountMap
                             ): (List[(Identifier, SchemeExp)], NameMap, CountMap) =
        bindings match
            case (v, e) :: rest =>
                count1(v, names, count) match
                    /* use old names, as with a let* the variable is not yet bound in its
                     * definition */
                    case (v1, names1, count1) =>
                        rename(e, names, count1) match
                            case (e1, count2) =>
                                renameLetStarBindings(rest, names1, count2) match
                                    case (rest1, names2, count3) =>
                                        ((v1, e1) :: rest1, names2, count3)
            case Nil => (Nil, names, count)

    def count1(
                  variable: Identifier,
                  names: NameMap,
                  count: CountMap
              ): (Identifier, NameMap, CountMap) =
        val c: Int = count.get(variable.name) match
            case Some(x) => x + 1
            case None    => 0
        val n = names.getOrElse(variable.name, variable.name) //s"_$variable$c")
        (Identifier(n, variable.idn), names, count + (variable.name -> c)) // + (variable.name -> n), count + (variable.name -> c))

    /** Same as count1 but for a list of variables */
    def countl(
                  variables: List[Identifier],
                  names: NameMap,
                  count: CountMap
              ): (List[Identifier], NameMap, CountMap) =
        variables.foldLeft((List[Identifier](), names, count))((st: (List[Identifier], NameMap, CountMap), v: Identifier) =>
            st match {
                case (l, ns, cs) =>
                    count1(v, ns, cs) match {
                        case (v1, ns1, cs1) => (v1 :: l, ns1, cs1)
                    }
            }
        ) match
            case (l, ns, cs) => (l.reverse, ns, cs)

    def renameIndex(exp: SchemeExp): SchemeExp =
        renameIndex(exp, List[String]()) match
            case (e, _) => e

    def getNewName(name: Option[String], names: List[String]): Option[String] =
        if names.isEmpty then
            return None
        names.indexOf(name.getOrElse("")) match
            case -1 => name
            case i: Int => Some(i.toString)

    def renameIndex(
                  exp: SchemeExp,
                  names: List[String],
          ): (SchemeExp, List[String]) = exp match
        case SchemeLambda(name, args, body, annotation, pos) =>
            var names1: List[String] = args.map(_.name).reverse.appendedAll(names)
            renameListIndex(body, names1) match
                        case (body1, names2) => (SchemeLambda(getNewName(name, names), List(Identifier("", NoCodeIdentity)), body1, annotation, pos), names)
        case SchemeFuncall(f, args, pos) =>
            renameIndex(f, names) match
                case (f1, names2) =>
                    renameListIndex(args, names) match
                        case (args1, names3) => (SchemeFuncall(f1, args1, pos), names)
        case SchemeIf(cond, cons, alt, pos) =>
            renameIndex(cond, names) match
                case (cond1, names1) =>
                    renameIndex(cons, names) match
                        case (cons1, names2) =>
                            renameIndex(alt, names) match
                                case (alt1, names3) => (SchemeIf(cond1, cons1, alt1, pos), names)
        case SchemeLet(bindings, body, pos) =>
            countlIndex(bindings.map(_._1), names) match
                case (variables, names1) =>
                    renameListIndex(bindings.map(_._2), names) match
                        case (exps, names2) =>
                            renameListIndex(body, names1) match
                                case (body1, names3) => (SchemeLet(variables.zip(exps), body1, pos) ,names)
        case SchemeLetStar(bindings, body, pos) =>
            renameLetStarBindingsIndex(bindings, names) match
                case (bindings1, names1) =>
                    renameListIndex(body, names1) match
                        case (body1, count2) => (SchemeLetStar(bindings1, body1, pos), names)
        case SchemeLetrec(bindings, body, pos) =>
            countlIndex(bindings.map(_._1), names) match
                case (variables, names1) =>
                    renameListIndex(bindings.map(_._2), names1) match
                        case (exps, count2) =>
                            renameListIndex(body, names1) match
                                case (body1, count3) => (SchemeLetrec(variables.zip(exps), body1, pos), names)
        case SchemeSet(variable, value, pos) =>
            renameIndex(value, names) match
                case (value1, count1) =>
                    (SchemeSet(getNewName(Some(variable.name), names) match {
                        case Some(n) => Identifier(n, variable.idn)
                        case None    => variable
                    },
                        value1,
                        pos
                    ),
                        count1
                    )
        case SchemeBegin(body, pos) =>
            renameListIndex(body, names) match
                case (body1, count1) => (SchemeBegin(body1, pos), names)
        case SchemeAssert(exp, pos) =>
            renameIndex(exp, names) match
                case (exp1, count1) => (SchemeAssert(exp1, pos), names)
        case SchemeVar(id) =>
            getNewName(Some(id.name), names) match
                case Some(n) => (SchemeVar(Identifier(n, id.idn)), names)
                case None    => (SchemeVar(Identifier(id.name, id.idn)), names) /* keep original name */
        case SchemeValue(v, pos) =>
            (SchemeValue(v, pos), names)
        case _ => throw new Exception(s"Unhandled expression in renamer: $exp")

    /** Renames a list of expressions executed sequentially (eg. within a begin) */
    def renameListIndex(
                      exps: List[SchemeExp],
                      names: List[String],
                  ): (List[SchemeExp], List[String]) = exps match
        case exp :: rest =>
            val (exp1, names1) = renameIndex(exp, names)
            val (rest1, names2) = renameListIndex(rest, names)
            (exp1 :: rest1, names)
        case Nil => (Nil, names)

    def renameLetStarBindingsIndex(
                                 bindings: List[(Identifier, SchemeExp)],
                                 names: List[String],
                             ): (List[(Identifier, SchemeExp)], List[String]) =
        bindings match
            case (v, e) :: rest =>
                count1Index(v, names) match
                    case (v1, names1) =>
                        renameIndex(e, names) match
                            case (e1, names2) =>
                                renameLetStarBindingsIndex(rest, v.name :: names) match
                                    case (rest1, names2) =>
                                        ((v1, e1) :: rest1, names2)
            case Nil => (Nil, names)

    def count1Index(
                  variable: Identifier,
                  names: List[String]
              ): (Identifier, List[String]) =
        val c = getNewName(Some(variable.name), names.reverse) match
            case Some(x) => if x.forall(_.isDigit) then x else 0
            case None    => 0
        val n = c.toString //s"_$variable$c")
        (Identifier(n, variable.idn), names) // + (variable.name -> n), count + (variable.name -> c))

    /** Same as count1 but for a list of variables */
    def countlIndex(
                  variables: List[Identifier],
                  names: List[String],
              ): (List[Identifier], List[String]) =
        variables.foldLeft((List[Identifier](), names))((st: (List[Identifier], List[String]), v: Identifier) =>
            st match {
                case (l, ns) =>
                    count1Index(v, v.name :: ns) match {
                        case (v1, ns1) => (v1 :: l, ns1)
                    }
            }
        ) match
            case (l, ns) => (l.reverse, ns)