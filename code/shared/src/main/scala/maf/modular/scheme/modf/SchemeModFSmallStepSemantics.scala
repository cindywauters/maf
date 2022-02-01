package maf.modular.scheme.modf

import maf.core._
import maf.core.worklist.{LIFOWorkList, WorkList}
import maf.language.scheme._
import maf.util.MonoidImplicits._
import maf.util.benchmarks.Timeout

trait SmallStepModFSemantics extends BaseSchemeModFSemanticsIdentity:
    // defining the intra-analysis
    override def intraAnalysis(cmp: Component): SmallStepIntra
    trait SmallStepIntra extends IntraAnalysis with SchemeModFSemanticsIntra:
        // the intermediate states in the intra-analysis
        sealed trait State
        case class EvalState(
            exp: SchemeExp,
            env: Env,
            cnt: Kont)
            extends State
        case class KontState(vlu: Value, cnt: Kont) extends State
        case class CallState(
            fexp: SchemeFuncall,
            fval: Value,
            args: List[(SchemeExp, Value)],
            cnt: Kont)
            extends State
        // the frames used to build the continuation
        type Kont = List[Frame]
        sealed trait Frame
        case class SeqFrame(exps: List[SchemeExp], env: Env) extends Frame
        case class SetFrame(id: Identifier, env: Env) extends Frame
        case class IfFrame(
            csq: SchemeExp,
            alt: SchemeExp,
            env: Env)
            extends Frame
        case class LetFrame(
            id: Identifier,
            bds: List[(Identifier, SchemeExp)],
            done: List[(Identifier, Value)],
            bdy: List[SchemeExp],
            env: Env)
            extends Frame
        case class LetStarFrame(
            id: Identifier,
            bds: List[(Identifier, SchemeExp)],
            bdy: List[SchemeExp],
            env: Env)
            extends Frame
        case class LetrecFrame(
            id: Identifier,
            bds: List[(Identifier, SchemeExp)],
            bdy: List[SchemeExp],
            env: Env)
            extends Frame
        case class ArgsFrame(
            fexp: SchemeFuncall,
            fval: Value,
            curExp: SchemeExp,
            toEval: List[SchemeExp],
            args: List[(SchemeExp, Value)],
            env: Env)
            extends Frame
        case class FunFrame(
            fexp: SchemeFuncall,
            args: List[SchemeExp],
            env: Env)
            extends Frame

        // the main analyze method
        def analyzeWithTimeout(timeout: Timeout.T): Unit =
            // determine the initial state
            val initialState = EvalState(fnBody, fnEnv, Nil)
            // standard worklist algorithm
            var work: WorkList[State] = LIFOWorkList[State](initialState)
            var visited = Set[State]()
            var result = lattice.bottom
            while work.nonEmpty do
                val state = work.head
                work = work.tail
                state match
                    case KontState(vlu, Nil) =>
                      result = lattice.join(result, vlu)
                    case _ if !visited.contains(state) =>
                      val successors = step(state)
                      work = work.addAll(successors)
                      visited += state
                    case _ => () // already visited this state
            writeResult(result)
        // stepping a state
        private def step(state: State): Set[State] = state match
            case EvalState(exp, env, cnt) =>
              eval(exp, env, cnt)
            case KontState(vlu, _) if lattice.isBottom(vlu) =>
              Set.empty
            case KontState(vlu, cnt) =>
              val frm = cnt.head
              continue(frm, vlu, cnt.tail)
            case CallState(fexp, fval, args, cnt) =>
              val result = applyFun(fexp, fval, args, fexp.idn.pos)
              Set(KontState(result, cnt))
        // eval
        private def eval(
            exp: SchemeExp,
            env: Env,
            cnt: Kont
          ): Set[State] = exp match
            case SchemeValue(value, _) =>
              val result = evalLiteralValue(value, exp)
              Set(KontState(result, cnt))
            case lambda: SchemeLambdaExp =>
              val result = newClosure(lambda, env)
              Set(KontState(result, cnt))
            case SchemeVar(id) =>
              val result = lookup(id, env)
              Set(KontState(result, cnt))
            case SchemeBegin(exps, _) =>
              evalSequence(exps, env, cnt)
            case SchemeSet(id, vexp, _) =>
              val frm = SetFrame(id, env)
              Set(EvalState(vexp, env, frm :: cnt))
            case SchemeIf(prd, csq, alt, _) =>
              val frm = IfFrame(csq, alt, env)
              Set(EvalState(prd, env, frm :: cnt))
            case SchemeLet(bindings, body, _) =>
              evalLet(bindings, Nil, body, env, cnt)
            case SchemeLetStar(bindings, body, _) =>
              evalLetStar(bindings, body, env, cnt)
            case SchemeLetrec(bindings, body, _) =>
              val extEnv = bindings.foldLeft(env) { case (env2, (id, _)) =>
                bind(id, env2, lattice.bottom)
              }
              evalLetrec(bindings, body, extEnv, cnt)
            case call @ SchemeFuncall(fexp, args, _) =>
              val frm = FunFrame(call, args, env)
              Set(EvalState(fexp, env, frm :: cnt))
            case SchemeAssert(exp, _) =>
              evalAssert(exp, env, cnt)
            case _ =>
              throw new Exception(s"Unsupported Scheme expression: $exp")
        private def evalSequence(
            exps: List[SchemeExp],
            env: Env,
            cnt: Kont
          ): Set[State] =
          if exps.tail.isEmpty then Set(EvalState(exps.head, env, cnt))
          else
              val frm = SeqFrame(exps.tail, env)
              Set(EvalState(exps.head, env, frm :: cnt))
        private def evalLet(
            bindings: List[(Identifier, SchemeExp)],
            done: List[(Identifier, Value)],
            body: List[SchemeExp],
            env: Env,
            cnt: Kont
          ): Set[State] = bindings match
            case Nil =>
              val extEnv = bind(done, env)
              evalSequence(body, extEnv, cnt)
            case (id, vexp) :: rest =>
              val frm = LetFrame(id, rest, done, body, env)
              Set(EvalState(vexp, env, frm :: cnt))
        private def evalLetStar(
            bindings: List[(Identifier, SchemeExp)],
            body: List[SchemeExp],
            env: Env,
            cnt: Kont
          ): Set[State] = bindings match
            case Nil => evalSequence(body, env, cnt)
            case (id, vexp) :: rest =>
              val frm = LetStarFrame(id, rest, body, env)
              Set(EvalState(vexp, env, frm :: cnt))
        private def evalLetrec(
            bindings: List[(Identifier, SchemeExp)],
            body: List[SchemeExp],
            env: Env,
            cnt: Kont
          ): Set[State] = bindings match
            case Nil => evalSequence(body, env, cnt)
            case (id, vexp) :: rest =>
              val frm = LetrecFrame(id, rest, body, env)
              Set(EvalState(vexp, env, frm :: cnt))
        private def evalArgs(
            fexp: SchemeFuncall,
            fval: Value,
            toEval: List[SchemeExp],
            ags: List[(SchemeExp, Value)],
            env: Env,
            cnt: Kont
          ): Set[State] = toEval match
            case Nil => Set(CallState(fexp, fval, ags.reverse, cnt))
            case exp :: rest =>
              val frm = ArgsFrame(fexp, fval, exp, rest, ags, env)
              Set(EvalState(exp, env, frm :: cnt))
        private def evalAssert(
            exp: SchemeExp,
            env: Env,
            cnt: Kont
          ): Set[State] =
          Set(KontState(lattice.void, cnt))
        // continue
        private def continue(
            frm: Frame,
            vlu: Value,
            cnt: Kont
          ): Set[State] = frm match
            case SeqFrame(exps, env) =>
              evalSequence(exps, env, cnt)
            case SetFrame(id, env) =>
              assign(id, env, vlu)
              Set(KontState(lattice.void, cnt))
            case IfFrame(csq, alt, env) =>
              conditional(vlu, Set(EvalState(csq, env, cnt)), Set(EvalState(alt, env, cnt)))
            case LetFrame(id, rest, done, body, env) =>
              evalLet(rest, (id, vlu) :: done, body, env, cnt)
            case LetStarFrame(id, rest, body, env) =>
              evalLetStar(rest, body, bind(id, env, vlu), cnt)
            case LetrecFrame(id, rest, body, env) =>
              assign(id, env, vlu)
              evalLetrec(rest, body, env, cnt)
            case FunFrame(fexp, args, env) =>
              evalArgs(fexp, vlu, args, Nil, env, cnt)
            case ArgsFrame(fexp, fval, curExp, toEval, args, env) =>
              val newArgs = (curExp, vlu) :: args
              evalArgs(fexp, fval, toEval, newArgs, env, cnt)

    override def configString(): String = super.configString() + "\n  applying small-step ModF Scheme semantics"
