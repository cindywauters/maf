package maf.language.scheme

import maf.core.*
import maf.core.Label
import maf.language.change.ChangeExp
import maf.modular.incremental.IncrementalModAnalysis
import maf.core.Expression
import maf.modular.incremental.update.{IncrementalModAnalysisWithUpdateTwoVersions, IncrementalUpdateDatastructures, SchemeChangeRenamerForPatterns}
import maf.modular.scheme.modf.SchemeModFComponent

import scala.::
import scala.annotation.tailrec

type ReanalysisList     = List[(Option[Expression], Option[Expression])]
type RenamingsList      = List[((Expression, Expression), (Boolean, Map[Identifier, Identifier]))]
type IfsList            = List[((SchemeIf, SchemeIf),  List[Identifier], (Expression, Expression))]
type BindingsWithScopes = Map[Expression, (Identifier, Map[String, Identifier])]
type BindingTuples      = (Expression, (Identifier, Map[String, Identifier]))
type ScopeChanges       = Map[BindingTuples, BindingTuples]

class SchemeChangePatterns:

    var reanalyse: ReanalysisList = List()
    var rename: RenamingsList = List()
    var needed_prims: List[Identifier] = List()
    var ifs: IfsList = List()
    var inserts: List[Expression] = List()
    var deletes: List[Expression] = List()
    var scopeChanges: ScopeChanges = Map()
    var maybeReanalyse: Map[Expression, (Option[Expression], Option[Expression])] = Map()
    var renamedBindingsIds: Map[Identifier, Identifier] = Map()
    var allOldScopes: BindingsWithScopes = Map()
    var allNewScopes: BindingsWithScopes = Map()

    // find all the changed expressions and create a set with all the old and new expressions
    def findAllChangedExpressions(expr: Expression): Set[ChangeExp[Expression]] = expr match
        case e: ChangeExp[Expression] => Set(e) // Assumption: change expressions are not nested.
        case e                        =>  e.subexpressions.asInstanceOf[List[Expression]].flatMap(findAllChangedExpressions).toSet


    // Get a list of all the variables that are declared within an expression (through lambdas, let, letrec, or let*)
    def findAllVarsInOrder(exp: Expression): List[Identifier] = exp match
        // In case of lambda, add the arguments and look at the body for more
        case SchemeLambda(name, args, body, annotation, pos) =>
            args.map(arg => arg).appendedAll(body.flatMap(e => findAllVarsInOrder(e)))
        // In case of let, let* and letrec, take the names of the bindings and look in the body for more
        case SchemeLet(bindings, body, pos) =>
            val bindingsVars = bindings.flatMap(binding => findAllVarsInOrder(binding._2))
            val bodyVars = body.flatMap(e => findAllVarsInOrder(e))
            bindings.map(binding => binding._1).appendedAll(bindingsVars).appendedAll(bodyVars)
        case SchemeLetStar(bindings, body, pos) =>
            val bindingsVars = bindings.flatMap(binding => findAllVarsInOrder(binding._2))
            val bodyVars = body.flatMap(e => findAllVarsInOrder(e))
            bindings.map(binding => binding._1).appendedAll(bindingsVars).appendedAll(bodyVars)
        case SchemeLetrec(bindings, body, pos) =>
            val bindingsVars = bindings.flatMap(binding => findAllVarsInOrder(binding._2))
            val bodyVars = body.flatMap(e => findAllVarsInOrder(e))
            bindings.map(binding => binding._1).appendedAll(bindingsVars).appendedAll(bodyVars)
        // In case of a list of expressions, look at each expression individually
        case exps: List[_] =>
            exps.flatMap(e => findAllVarsInOrder(exps))
        // In case of a single expression, look at the subexpressions (if there are any)
        case e: Expression =>
            if e.subexpressions.nonEmpty then
                e.subexpressions.flatMap(e => findAllVarsInOrder(e))
            else  List()
        case e: _ =>
            List()


    def checkRenamingsVariables(oldexp: Expression, newexp: Expression): (Boolean, Map[Identifier, Identifier]) =
        (oldexp, newexp) match
            case (oe: SchemeExp, ne: SchemeExp) =>
                if oldexp.height != newexp.height then // optimization, two expressions can not be alpha-equivalent if they don't have the same height
                    return (false, Map())
                if oldexp.label != newexp.label then // optimization, two expressions can not be alpha-equivalent if they don't have the same label
                    return (false, Map())
                try {
                    val variablesOld = findAllVarsInOrder(oe)
                    val variablesNew = findAllVarsInOrder(ne)
                    if variablesOld.length != variablesNew.length then
                        return (false, Map())
                   /* val renamedOld = SchemeRenamer.rename(oe)
                    val renamedNew = SchemeRenamer.rename(ne)
                    val variablesRenamedOld = findAllVarsInOrder(renamedOld)
                    val variablesRenamedNew = findAllVarsInOrder(renamedNew)
                    var mappedVars = variablesNew.map(e => e.name).zip(variablesOld.map(e => e.name)).toMap
                    val mappedRenamedVars = variablesRenamedNew.map(e => e.name).zip(variablesRenamedOld.map(e => e.name)).toMap*/
                    val mappedIdentifiers = variablesNew.zip(variablesOld).toMap
                    if SchemeChangeRenamerForPatterns.renameIndex(oldexp.asInstanceOf[SchemeExp]).eql(SchemeChangeRenamerForPatterns.renameIndex(newexp.asInstanceOf[SchemeExp])) then
                        return (true, mappedIdentifiers)
                } catch {
                    case e: _ => return (false, Map())
                }
        return (false, Map())

    def checkForRenamingParameter(exp: SchemeExp): Set[((Expression, Expression), (Boolean, Map[Identifier, Identifier]))] =
        val changedExpr = findAllChangedExpressions(exp)
        val changedExprAsPairs = changedExpr.map(e => (e.old, e.nw))
        // toList necessary because sets cannot contain duplicate items
        val renamings: List[(Boolean, Map[Identifier, Identifier])] = changedExprAsPairs.toList.map(e => checkRenamingsVariables(e._1, e._2))
        changedExprAsPairs.zip(renamings)

    def compareRenamingsBindings(oldname: Identifier, newname: Identifier, oldexp: SchemeExp, nwexp: SchemeExp): (Boolean, Map[Identifier, Identifier]) =
        var mappedVars = Map(newname.name -> oldname.name)
        var mappedIds = Map(newname -> oldname)
        (oldexp, nwexp) match
            case (ol: SchemeLambda, nl: SchemeLambda) =>
                val varsOld = findAllVarsInOrder(ol)
                val varsNew = findAllVarsInOrder(nl)
                mappedVars = mappedVars ++ varsNew.map(_.name).zip(varsOld.map(_.name)).toMap
                mappedIds = mappedIds ++ varsNew.zip(varsOld).toMap
            case _ =>
        try {
            if SchemeChangeRenamerForPatterns.renameIndex(oldexp, List(oldname.name))._1.eql(SchemeChangeRenamerForPatterns.renameIndex(nwexp,List(newname.name))._1) then
                (true, mappedIds)
            else
                (false, Map())
        } catch {
            case e: _ => return (false, Map())
        }

    // conditions are represented as a list as in case such as (not (...)) the arguments of not are used
    def comparePartialCondAndBranches(oldCond: List[SchemeExp], newCond: List[SchemeExp], oldTrue: SchemeExp, oldFalse: SchemeExp, newTrue: SchemeExp, newFalse: SchemeExp): Boolean =
        oldCond.zip(newCond).map(e => e._1.eql(e._2)).forall(e => e) && oldTrue.eql(newFalse) && newTrue.eql(oldFalse)

    def compareIfs(oldIf: SchemeIf, newIf: SchemeIf, prims: List[Identifier]): Option[(List[Identifier], (Expression, Expression))] =
        (oldIf.cond, newIf.cond) match
            case (oldFun: SchemeFuncall, newFun: SchemeFuncall) =>
                (oldFun.f, newFun.f) match
                    case (SchemeVar(oldId), _) if oldId.name == "not" =>
                        if comparePartialCondAndBranches(oldFun.args, List(newFun), oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                          //  println("119")
                            Some(List(), (oldFun.args.head, newFun))
                        else None
                    /*     case (_, SchemeVar(newId)) if newId.name == "not" =>
                           if comparePartialCondAndBranches(List(oldFun), newFun.args, oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                             println("124")
                             Some(prims.filter(e => e.name == "not"), (oldFun, newFun.args.head))
                           else None*/
                    case (SchemeVar(oldId), SchemeVar(newId)) =>
                        if oldId.name == ">=" && newId.name == "<" || oldId.name == ">" && newId.name == "<=" then //oldId.name == "<=" && newId.name == ">"|| oldId.name == ">" && newId.name == "<=" || oldId.name == ">=" && newId.name == "<" || oldId.name == "<" && newId.name == ">=" then
                            if comparePartialCondAndBranches(oldFun.args, newFun.args, oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                            //    println(prims)
                            //    println(newId.name)
                                return Some(prims.filter(e => e.name == newId.name), (oldFun, newFun))
                        None
                    case (_, _) =>
                        None
            case (oldFun: SchemeFuncall, newVal: _) =>
                oldFun.f match
                    case SchemeVar(oldId) =>
                        if oldId.name == "not" then
                            if comparePartialCondAndBranches(oldFun.args, List(newVal), oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                             //   println("148")
                                return Some(List(), (oldFun.args.head, newVal))
                        None
                    case _ => None
            /* case (oldVal: _, newFun: SchemeFuncall) =>
               newFun.f match
                 case SchemeVar(newId) =>
                   if newId.name == "not" then
                     if comparePartialCondAndBranches(List(oldVal), newFun.args, oldIf.cons, oldIf.alt, newIf.cons, newIf.alt) then
                       println("139")
                       return Some(prims.filter(e => e.name == "not"), (oldVal, newFun.args.head))
                   None
                 case _ => None*/
            case (e1: _, e2: _) => None

    def checkPossibilityRenaming(oe: Expression, ne: Expression): Boolean =
        oe.getClass == ne.getClass && oe.subexpressions.size == ne.subexpressions.size && oe.height == ne.height

    def findLowestChangedSubExpressions(old: Expression, nw: Expression): Unit =
        if old.subexpressions.isEmpty && nw.subexpressions.isEmpty then
            reanalyse = reanalyse.::((Some(old), Some(nw)))
        if old == nw then
            return
        else
            (old, nw) match
                case (oe: Identifier, ne: Identifier) =>
                    if renamedBindingsIds.contains(oe) then
                        rename = rename.::((oe, ne), (true, Map(ne -> oe)))
                        return
                case (oldIf: SchemeIf, newIf: SchemeIf) =>
                    val maybeSwapped = compareIfs(oldIf, newIf, needed_prims)
                    maybeSwapped match
                        case Some((added, conds)) =>
                            ifs = ifs.::((oldIf -> newIf), added, conds)
                            return
                        case None =>
                            if oldIf.eql(newIf) then
                                rename = rename.::((oldIf, newIf), (true, Map()))
                                return
                case (ol: SchemeLettishExp, nl: SchemeLettishExp) =>
                    val renamed = checkRenamingsVariables(ol, nl)
                    if renamed._1 then
                        rename = rename.::((ol, nl), renamed)
                        return
                    else if ol.subexpressions.forall(o => !nl.subexpressions.exists(n => o.idn == n.idn)) then
                        reanalyse = reanalyse.::(Some(ol), Some(nl))
                        return
                /*   else if renamed._1 then
                     ol.body.zip(nl.body).foreach(b => findLowestChangedSubExpressions(b._1, b._2))
                     return*/
                case (ol: SchemeLambda, nl: SchemeLambda) =>
                    val renamed = checkRenamingsVariables(ol, nl)
                    if renamed._1 then
                        rename = rename.::((ol, nl), renamed)
                        return
                    else if ol.subexpressions.forall(o => !nl.subexpressions.exists(n => o.idn == n.idn)) then
                        reanalyse = reanalyse.::(Some(ol), Some(nl))
                        return
                /* else if renamed._1 then
                   ol.body.zip(nl.body).foreach(b => findLowestChangedSubExpressions(b._1, b._2))
                   return */
                case (oldexp: SchemeExp, newexp: SchemeExp) if oldexp.subexpressions.forall(o => !newexp.subexpressions.exists(n => o.idn == n.idn))  =>
                    reanalyse = reanalyse.::(Some(oldexp), Some(newexp))
                    return
                case (oldfun: SchemeFuncall, newfun: SchemeFuncall) if oldfun.height == 2 || newfun.height == 2 =>
                    reanalyse = reanalyse.::(Some(oldfun), Some(newfun))
                    return
                case _ =>
            var addToMaybe: List[Expression] = List()
            old.subexpressions.foreach(oe =>
                nw.subexpressions.find(ne => oe.idn == ne.idn && ne.label == oe.label) match
                    case Some(ne) =>
                        if oe.subexpressions.exists(oe => ne.subexpressions.exists(ne => oe.idn == ne.idn)) then
                            findLowestChangedSubExpressions(oe, ne)
                        else if oe != ne then
                            findLowestChangedSubExpressions(oe, ne)
                    case None =>
                        nw.subexpressions.find(ne => oe.idn == ne.idn) match
                            case Some(ne) =>
                                if oe.subexpressions.exists(oe => ne.subexpressions.exists(ne => oe.idn == ne.idn)) then
                                    findLowestChangedSubExpressions(oe, ne)
                                else if oe != ne then
                                    findLowestChangedSubExpressions(oe, ne)
                            case None =>
                                deletes = deletes.::(oe)
                                addToMaybe = addToMaybe.::(oe))
            nw.subexpressions.foreach(ne =>
                if !old.subexpressions.exists(oe => oe.idn == ne.idn) then
                    inserts = inserts.::(ne)
                        addToMaybe = addToMaybe.::(ne))
            addToMaybe.foreach(maybe =>
                maybeReanalyse = maybeReanalyse + (maybe -> (Some(old), Some(nw))))


    // Returns a list of expressions that needs to be reanalysed (old and new), and a list of tuples of expressions that are just renamings together with their mappings
    def comparePrograms(old: SchemeExp, nw: SchemeExp, analysis: Option[IncrementalModAnalysisWithUpdateTwoVersions[_]] = None, refactoring: Boolean = true): differentChanges =
        allOldScopes = findLexicalScopes(old)
        allNewScopes = findLexicalScopes(nw)
        (old, nw) match
            case (oldlet: SchemeLettishExp, newlet: SchemeLettishExp) =>
                var relatedBindings: Map[Expression, Expression] = Map()
                oldlet.bindings.foreach(oe =>
                    newlet.bindings.find(ne => (oe != ne) && (oe._2.idn == ne._2.idn) && oe._2.idn != NoCodeIdentity) match
                        case Some(ne) =>
                            if oe._1.name != ne._1.name then
                                val renamed = compareRenamingsBindings(oe._1, ne._1, oe._2, ne._2)
                                if renamed._1 then
                                    rename = rename.::((oe._2, ne._2), renamed)
                                        renamedBindingsIds = renamedBindingsIds + (oe._1 -> ne._1)
                                else
                                    relatedBindings = relatedBindings + (oe._2 -> ne._2)
                            else
                                relatedBindings = relatedBindings + (oe._2 -> ne._2)
                        case _       =>
                    if !newlet.bindings.exists(ne => oe._2.idn == ne._2.idn) then
                        deletes = deletes.::(oe._2)
                    if oe._1.idn.idn.tag != Position.noTag then
                        needed_prims = needed_prims.::(oe._1))
                newlet.bindings.foreach(ne =>
                    if !oldlet.bindings.exists(oe => oe._2.idn == ne._2.idn) then
                        inserts = inserts.::(ne._2)
                    if ne._1.idn.idn.tag != Position.noTag && !needed_prims.contains(ne._1) then
                        needed_prims = needed_prims.::(ne._1))
                relatedBindings.foreach(related =>
                    findLowestChangedSubExpressions(related._1, related._2))
                if refactoring then
                    deletes.foreach(deleted =>
                    inserts.find(i => i.eql(deleted)) match
                        case Some(inserted) =>
                            val oldEnv = allOldScopes.get(deleted)
                            val newEnv = allNewScopes.get(inserted)
                            (oldEnv, newEnv) match
                                case (Some(oenv: (Identifier, Map[String, Identifier])), Some(nenv: (Identifier, Map[String, Identifier]))) =>
                                    var oenvOnly = oenv._2
                                    var nenvOnly = nenv._2
                                    var oldIdentifier = oenv._1
                                    var newIdentifier = nenv._1
                                    if oenvOnly.contains(oldIdentifier.name) && nenvOnly.contains(newIdentifier.name) then // recursive case
                                        oenvOnly = oenvOnly.filter((k, v) => k != oldIdentifier.name)
                                            nenvOnly = nenvOnly.filter((k, v) => k != newIdentifier.name)
                                    if oenvOnly == nenvOnly && oldIdentifier.name == newIdentifier.name then
                                        scopeChanges = scopeChanges + ((deleted, oenv) -> (inserted, nenv))
                                case _ =>
                        case _ =>)
              //  println("reanalyse")
              //  reanalyse.foreach(println)
                maybeReanalyse.foreach(e => e match
                    case (a: Identifier, (Some(oldEncapsulating), Some(nwEncapsulating))) =>
                    case (a: SchemeExp, (Some(oldEncapsulating: Expression), Some(nwEncapsulating: Expression))) if !refactoring || !scopeChanges.exists(e => e._1._1 == a) =>
                        reanalyse = reanalyse.::(Some(oldEncapsulating), Some(nwEncapsulating))
                    case _ =>
                )
                if refactoring && (rename.nonEmpty || scopeChanges.nonEmpty) then
                    checkAffectedEnvs()
                reanalyse = reanalyse//.appendedAll(deletes)//.appendedAll(inserts)
                println(reanalyse.size)
                println("rename")
                println(rename.size)
                println("ifs")
                println(ifs.size)
                println("scope changes")
                println(scopeChanges.size)
                differentChanges(reanalyse, rename, ifs, scopeChanges, allNewScopes)
            case _ => differentChanges(reanalyse, rename, ifs, scopeChanges, allNewScopes)

    def checkAffectedEnvs(): Unit =
        var allToCheckVar: Map[Identifier, Identifier] = rename.flatMap(_._2._2).toMap ++ scopeChanges.map((old, nw) => (nw._2._1, old._2._1)).toMap
        allOldScopes.foreach((e, env) =>
            if !allNewScopes.contains(e) then
                allNewScopes.find((ne, nenv) => e.idn == ne.idn && e.label == ne.label) match
                    case Some(newid, newenv) =>
                        val allToCheckOld = env._2.filter(fv => allToCheckVar.exists(e => e._2 == fv._2))
                        val allToCheckNew = newenv._2.filter(fv => allToCheckVar.contains(fv._2))
                        if allToCheckOld.size != allToCheckNew.size then
                            reanalyse = reanalyse.::(Some(e), Some(e))
                    case _ =>)

    var up = new IncrementalUpdateDatastructures

    @tailrec
    final def findEquivalent(expr: Expression, in: List[Expression]): Option[Expression] = in match
        case List() => None
        case first :: _ if first.idn == expr.idn && first.getClass == expr.getClass => Some(first)
        case first :: second :: rest if first.idn.idn.line <= expr.idn.idn.line && expr.idn.idn.line < second.idn.idn.line =>
            val subs: List[Expression] =  up.findAllSubExps(first)
            findEquivalent(expr, subs.tail)
        case first :: rest => findEquivalent(expr, rest)

    def findEquivalentLambdas(lams: List[Expression], program: Expression): List[(Expression, Option[Expression])] =
        var matchingLams: List[(SchemeLambda, SchemeLambda)] = List()
        program match
            case let: SchemeLettishExp =>
                val bindings = let.bindings.map(_._2)
                lams.map(e => (e, findEquivalent(e, bindings)))

    def findEquivalentLambdasInFunctionOfScopes(lams: List[Expression]): List[(Expression, Option[Expression])] =
        lams.collect {
            case lam: SchemeLambdaExp =>
                allNewScopes.get(lam) match
                    case None =>
                        allNewScopes.find(otherLam => otherLam._1.idn == lam.idn) match
                            case Some(otherLam) => (lam, Some(otherLam._1))
                            case None           => (lam, None)
                    case Some(otherLam)     => (lam, Some(lam))
        }

    def findEnclosingLambda(expr: Expression, nw: Boolean = true): Option[Expression] =
        val scopeToFind = if nw then allNewScopes else allOldScopes
        val possibilities: List[Expression] = scopeToFind.collect {
            case (lam, _) if expr.idn == NoCodeIdentity && up.findAllSubExps(lam).contains(expr) =>
                lam
            case (lam, _) if lam.idn.idn.line <= expr.idn.idn.line && up.findAllSubExps(lam).contains(expr) =>
                lam
        }.toList
        if possibilities.isEmpty then
            None
        else
            Some(possibilities.minBy(_.height))


    def allBindingsInProgramIdSchemeExp(expr: Expression): List[(Identifier, Expression)] =
        if expr.subexpressions.isEmpty && expr.height == 1 then
            List()
      //  else if expr.subexpressions.isEmpty then
      //      List()
        else expr match
            case let: SchemeLettishExp => let.bindings.appendedAll(let.bindings.flatMap(b => allBindingsInProgramIdSchemeExp(b._2))).appendedAll(let.body.flatMap(allBindingsInProgramIdSchemeExp))
            case _ => expr.subexpressions.flatMap(e => allBindingsInProgramIdSchemeExp(e))

    def allBindingsInprogram(expr: Expression): Map[Expression, Identifier] =
        allBindingsInProgramIdSchemeExp(expr).map(_.swap).toMap

    def findLexicalScopes(expr: Expression, currentScope: Map[String, Identifier] = Map(), currentBinding: Identifier = Identifier("", NoCodeIdentity)): BindingsWithScopes =
        var newScope = currentScope
        var toReturn: BindingsWithScopes = Map()
        expr match
            case let: SchemeLet =>
                let.bindings.foreach(b =>
                    newScope = newScope + (b._1.name -> b._1)
                        toReturn = toReturn ++ findLexicalScopes(b._2, currentScope, b._1))
                toReturn ++ let.body.flatMap(exp => findLexicalScopes(exp, newScope))
            case let: SchemeLetrec =>
                let.bindings.foreach(b =>
                    newScope = newScope + (b._1.name -> b._1))
                let.bindings.foreach(b =>
                    toReturn = toReturn ++ findLexicalScopes(b._2, newScope, b._1))
                toReturn ++ let.body.flatMap(exp => findLexicalScopes(exp, newScope))
            case let: SchemeLetStar =>
                let.bindings.foreach(b =>
                    toReturn = toReturn ++ findLexicalScopes(b._2, newScope, b._1)
                        newScope = newScope + (b._1.name -> b._1))
                toReturn ++ let.body.flatMap(exp => findLexicalScopes(exp, newScope))
            case lam: SchemeLambdaExp =>
                newScope = currentScope ++ lam.args.map(id => (id.name, id)).toMap
                Map(lam -> (currentBinding, currentScope.filter(v => lam.fv.contains(v._1)))) ++ lam.body.flatMap(findLexicalScopes(_, newScope)).toMap
            case _ =>
                if expr.subexpressions.nonEmpty then
                    expr.subexpressions.flatMap(findLexicalScopes(_, newScope)).toMap
                else
                    Map()


    @tailrec
    final def findLatestInScope(toFind: Map[String, Option[Identifier]], expr: Expression, program: List[(Identifier, Expression)]): Map[String, Option[Identifier]] = program match
        case List() =>
            toFind
        case (id, binding) :: rest if toFind.contains(id.name) =>
            findLatestInScope(toFind + (id.name -> Some(id)), expr, rest)
        case (id, binding) :: rest if up.findAllSubExps(binding).contains(expr) =>
            val newBindings = up.findAllSubExps(binding).collect {
                // In case of let: let must not contain the expression itself (other expressions of let will be out of scope) and the beginning line of let must not be larger than the expression line (out of scope)
                case let: SchemeLet if let.idn.idn.line <= expr.idn.idn.line && !let.bindings.exists(b => b._2.eql(expr)) =>
                    let.bindings
                // Let*: bindings know of each other and can call eachother, but not ones that are defined later
                case letstar: SchemeLetStar if letstar.idn.idn.line <= expr.idn.idn.line =>
                    letstar.bindings.filter(e => e._2.idn.idn.line < expr.idn.idn.line)
                // Letrec: bindings can call each other even if they are defined later within the same letrec bindings
                case letrec: SchemeLetrec if letrec.idn.idn.line <= expr.idn.idn.line  =>
                    letrec.bindings
            }.flatten
            findLatestInScope(toFind, expr, rest.appendedAll(newBindings))
        case (id, binding) :: rest =>
            findLatestInScope(toFind, expr, rest)





case class differentChanges(reanalyse: ReanalysisList, renamings: RenamingsList, ifs: IfsList, scopeChanges: ScopeChanges, allLexicalEnvs: BindingsWithScopes)