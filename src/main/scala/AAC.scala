import scalaz.Scalaz._

/**
 * Implementation of Johnson's CESIK*Ξ machine with a global continuation store
 * (Johnson, James Ian, and David Van Horn. "Abstracting abstract control."
 * Proceedings of the 10th ACM Symposium on Dynamic languages. ACM, 2014).
 *
 * A difference with the paper is that both the call site (i.e., the expression)
 * and the value of every argument is stored in the context when stepping into a
 * function. The paper also contains some typos in the formalisation, which are
 * fixed here. The state exploration strategy is also frontier based, but
 * completely discards seen states upon modification of the continuation store.
 *
 * TODO: There still is a bug, preventing nqueens to converge
 * TODO: Subsumption is defined for every component of the machine, but not
 * used. Use it or remove the definitions.
 * TODO: Investigating AAC with a global value store might be interesting.
 */
case class AAC[Exp : Expression, Abs, Addr]
  (implicit ab: AbstractValue[Abs], abi: AbstractInjection[Abs],
    ad: Address[Addr], adi: AddressInjection[Addr])
    extends EvalKontMachine[Exp, Abs, Addr] {
  def name = "AAC"

  val primitives = new Primitives[Addr, Abs]()

  /**
   * A context is basically a continuation's address, is allocated when stepping
   * into a function body, and stores some information to have good precision:
   * the closure itself, the expressions and values of the arguments, and the
   * store at calling time.
   */
  case class Context(clo: (Exp, Environment[Addr]), argsv: List[(Exp, Abs)], σ: Store[Addr, Abs]) {
    def subsumes(that: Context) = {
      clo._1.equals(that.clo._1) && clo._2.subsumes(that.clo._2) && σ.subsumes(that.σ) &&
      argsv.zip(that.argsv).forall({ case ((e1, v1), (e2, v2)) => e1 == e2 && abs.subsumes(v1, v2) })
    }
  }

  /** A Kont is just an address in the continuation store */
  sealed abstract class Kont {
    def subsumes(that: Kont) : Boolean
  }
  /** It can be a context itself */
  case class KontCtx(ctx: Context) extends Kont {
    def subsumes(that: Kont) = that match {
      case KontCtx(ctx2) => ctx.subsumes(ctx2)
      case _ => false
    }
    override def toString = s"KontCtx(${ctx.clo._1}, ${ctx.argsv})"
  }
  /** Or the address of the empty continuation */
  object KontEmpty extends Kont {
    def subsumes(that: Kont) = that.equals(this)
    override def toString = "KontEmpty"
  }

  /** Actual frames are stored in local continuations */
  case class LocalKont(frames: List[Frame]) {
    def this() = this(List())
    def subsumes(that: LocalKont) = frames.zip(that.frames).forall({ case (f1, f2) => f1.subsumes(f2) })
    def isEmpty = frames.isEmpty
    def deconstruct = frames match {
      case List() => None
      case h :: t => Some((h, LocalKont(t)))
    }
    def push(frame: Frame): LocalKont = new LocalKont(frame :: frames)
  }

  /**
   * The continuation store maps continuation addresses to a set of
   * continuations, made of a local continuation and an address for the rest of
   * the continuation.
   *
   * TODO: This basically redefines Kontinuation.scala's KontStore, because the
   * codomain of the map is different (Set[Kont[KontAddr]] vs. Set[(LocalKont,
   * Kont)], where Kont is a different class). Unifying both definition would be
   * better.
   */
  case class KontStore(content: Map[Context, Set[(LocalKont, Kont)]]) {
    def this() = this(Map())
    def lookup(τ: Context): Set[(LocalKont, Kont)] = content.getOrElse(τ, Set())
    def extend(τ: Context, v: (LocalKont, Kont)): KontStore = {
      /*
      content.get(τ) match {
        case Some(vals) if !vals.contains(v) => println(s"Joining at $τ: $v + $vals")
        case _ => ()
      } */
      KontStore(content + (τ -> (lookup(τ) + v)))
    }
    def join(that: KontStore): KontStore = KontStore(content |+| that.content)
    /** Useful for debugging purposes, in order to have a visualization of the
      * kontinuation store */
    def toDotFile(file: String): Unit = {
      val graph = content.foldLeft(new Graph[Kont]())({ case (g, (τ, succs)) =>
        succs.foldLeft(g)({ case (g, (local, κ)) =>
          /* TODO: annotate edge with local continuation (but graph doesn't
           * support edge annotations yet) */
          g.addEdge(KontCtx(τ), κ)
        })
      })
      graph.toDotFile(file, {
        case KontCtx(τ) => τ.toString.take(40)
        case KontEmpty => "ε"
      }, x => "#FFFFFF")
    }
  }

  /**
   * The state of the machine contains the control component, the local store, a
   * local continuation and the address of the rest of the continuation
   */
  case class State(control: Control, σ: Store[Addr, Abs], ι: LocalKont, κ: Kont) {
    def this(exp: Exp) = this(ControlEval(exp, Environment.empty[Addr]().extend(primitives.forEnv)),
                               Store.initial[Addr, Abs](primitives.forStore), new LocalKont(), KontEmpty)
    override def toString() = control.toString(σ)
    def subsumes(that: State): Boolean = control.subsumes(that.control) && σ.subsumes(that.σ) && ι.subsumes(that.ι) && κ.subsumes(that.κ)

    /* TODO: There are a few functions inspecting the continuation (pop,
     * computeKont, kontCanBeEmpty). They should be factored into a single
     * function with the stack-walking mechanics, and some helper functions
     * telling what to do when encountering each kind of stack */

    /**
     * Administrative pop function, popping a frame from the stack. That
     * requires some exploration in the continuation store in case the local
     * continuation is empty. G is the set of continuations already visited, to
     * avoid looping indefinitely when there is a cycle in the continuation
     * store.
     */
    private def pop(ι: LocalKont, κ: Kont, kstore: KontStore, G: Set[Kont]): Set[(Frame, LocalKont, Kont)] = ι.deconstruct match {
      case None => κ match {
        case KontEmpty =>
          /* No local continuation and no continuation, the stack is empty and there is no top frame */
          Set()
        case KontCtx(τ) => {
          /* G2 contains the continuation addresses to which no local continuation are associated */
          val G2: Set[Kont] = kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set(κ)
              case Some(_) => Set[Kont]()
            }
          }).diff(G)
          val GuG2 = G.union(G2) /* This is the new visited set */
          /* For every continuation that has a local continuation associated to it, return
           * the top frame of this local continuation. To this, add the popped
           * frame for every continuation to which no local continuation are
           * associated. */
          kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set[(Frame, LocalKont, Kont)]()
              case Some((top, rest)) => Set((top, rest, κ))
            }
          }).union(G2.flatMap((κ) => pop(new LocalKont(), κ, kstore, GuG2)))
        }
      }
      case Some((top, rest)) =>
        /* Local continuation is not empty, just return the top frame */
        Set((top, rest, κ))
    }

    private def pop(ι: LocalKont, κ: Kont, kstore: KontStore): Set[(Frame, LocalKont, Kont)] =
      pop(ι, κ, kstore, Set())

    /** Debugging function that computes the full current continuation. It is very similar to pop. */
    private def computeKont(ι: LocalKont, κ: Kont, kstore: KontStore, G: Set[Kont]): Set[List[Frame]] = ι.deconstruct match {
      case None => κ match {
        case KontEmpty => Set(List())
        case KontCtx(τ) => {
          val G2: Set[Kont] = kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set(κ)
              case Some(_) => Set[Kont]()
            }
          }).diff(G)
          val GuG2 = G.union(G2)
          kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set[List[Frame]]()
              case Some((top, rest)) => computeKont(rest, κ, kstore, GuG2).map(k => top :: k)
            }
          }).union(G2.flatMap((κ) => computeKont(new LocalKont(), κ, kstore, GuG2)))
        }
      }
      case Some((top, rest)) => computeKont(rest, κ, kstore, G).map(k => top :: k)
    }

    private def computeKont(ι: LocalKont, κ: Kont, kstore: KontStore): Set[List[Frame]] =
      computeKont(ι, κ, kstore, Set())

    /**
     *  Checks whether the continuaton can be empty. That is, will the evaluation be
     * done once we reach a control state?
     */
    private def kontCanBeEmpty(ι: LocalKont, κ: Kont, kstore: KontStore, G: Set[Kont]): Boolean = ι.deconstruct match {
      case None => κ match {
        case KontEmpty => true
        case KontCtx(τ) => {
          val G2: Set[Kont] = kstore.lookup(τ).flatMap({
            case (ι, κ) => ι.deconstruct match {
              case None => Set(κ)
              case Some(_) => Set[Kont]()
            }
          }).diff(G)
          val GuG2 = G.union(G2)
          kstore.lookup(τ).map({
            case (ι, KontEmpty) => ι.isEmpty
            case (ι, κ) => ι.deconstruct match {
              case None => false
              case Some((top, rest)) => false
            }
          }).union(G2.map((κ) => kontCanBeEmpty(new LocalKont(), κ, kstore, GuG2))).foldLeft(false)((x, y) => x || y)
        }
      }
      case Some((top, rest)) => false
    }

    private def kontCanBeEmpty(ι: LocalKont, κ: Kont, kstore: KontStore): Boolean =
      kontCanBeEmpty(ι, κ, kstore, Set())

    /**
     * Integrate a set of actions to generate successor states, and returns
     * these states as well as the updated continuation store (which is global)
     */
    private def integrate(ι: LocalKont, κ: Kont, kstore: KontStore, v: Abs, actions: Set[Action[Exp, Abs, Addr]]): (Set[State], KontStore) =
      actions.foldLeft((Set[State](), kstore))({ (acc, act) =>
        val states = acc._1
        val kstore = acc._2
        act match {
          case ActionReachedValue(v, σ) => (states + State(ControlKont(v), σ, ι, κ), kstore)
          case ActionPush(e, frame, ρ, σ) => (states + State(ControlEval(e, ρ), σ, ι.push(frame), κ), kstore)
          case ActionEval(e, ρ, σ) => (states + State(ControlEval(e, ρ), σ, ι, κ), kstore)
          case ActionStepIn(clo, e, ρ, σ, argsv) => {
            val τ = Context(clo, argsv, σ)
            (states + State(ControlEval(e, ρ), σ, new LocalKont(), new KontCtx(τ)),
             kstore.extend(τ, (ι, κ)))
          }
          case ActionError(err) => (states + State(ControlError(err), σ, ι, κ), kstore)
        }})

    /**
     * Performs an evaluation step, relying on the given semantics (@param sem)
     */
    def step(kstore: KontStore, sem: Semantics[Exp, Abs, Addr]): (Set[State], KontStore) = control match {
      case ControlEval(e, ρ) => integrate(ι, κ, kstore, absi.bottom, sem.stepEval(e, ρ, σ))
      case ControlKont(v) if abs.isError(v) => (Set(), kstore)
      case ControlKont(v) => pop(ι, κ, kstore).foldLeft((Set[State](), kstore))((acc, popped) => {
        val (states, kstore1) = integrate(popped._2, popped._3, acc._2, v, sem.stepKont(v, σ, popped._1))
        (acc._1 ++ states, kstore1)
      })
      case ControlError(_) => (Set(), kstore)
    }

    /**
     * Checks whether this state, with the given continuation store, is a state
     * where the computation could be finished.
     */
    def halted(kstore: KontStore) = control match {
      case ControlEval(_, _) => false
      case ControlKont(v) => abs.isError(v) || (ι.isEmpty && kontCanBeEmpty(ι, κ, kstore))
      case ControlError(_) => true
    }
  }

  /**
   * Output of the machine
   */
  case class AACOutput(halted: Set[State], count: Int, t: Double, graph: Option[Graph[State]])
      extends Output[Abs] {
    def finalValues = halted.flatMap(st => st.control match {
      case ControlKont(v) => Set[Abs](v)
      case _ => Set[Abs]()
    })
    def containsFinalValue(v: Abs) = finalValues.exists(v2 => abs.subsumes(v2, v))
    def numberOfStates = count
    def time = t
    def toDotFile(path: String) = graph match {
      case Some(g) => g.toDotFile(path, _.toString.take(40),
        (s) => if (halted.contains(s)) { "#FFFFDD" } else { s.control match {
          case ControlEval(_, _) => "#DDFFDD"
          case ControlKont(_) => "#FFDDDD"
          case ControlError(_) => "#FF0000"
        }})
      case None =>
        println("Not generating graph because no graph was computed")
    }
  }

  /** Performs the frontier-based state exploration */
  @scala.annotation.tailrec
  private def loop(todo: Set[State], visited: Set[State],
    halted: Set[State], startingTime: Long, graph: Option[Graph[State]],
    kstore: KontStore, sem: Semantics[Exp, Abs, Addr]): AACOutput = {
    if (todo.isEmpty) {
      /* No more element to visit, outputs the result */
      AACOutput(halted, visited.size,
        (System.nanoTime - startingTime) / Math.pow(10, 9), graph)
    } else {
      /* Steps every state in the todo list, and merge all the resulting kstores
       * together */
      val (edges, kstore2) = todo.foldLeft((Set[(State, State)](), kstore))({ (acc, ς) =>
        ς.step(kstore, sem) match {
          case (next, kstore2) =>
            (acc._1 ++ next.map((ς2) => (ς, ς2)), acc._2.join(kstore2))
        }
      })
      if (kstore.equals(kstore2)) {
        /* Continuation store stayed the same, continue exploring the state space */
        loop(edges.map({ case (_, ς2) => ς2 }).diff(visited),
          visited ++ todo,
          halted ++ todo.filter((ς) => ς.halted(kstore)),
          startingTime,
          graph.map(_.addEdges(edges)),
          kstore2,
          sem)
      } else {
        /* Continuation store changed, discard set of seen states */
        loop(edges.map({ case (_, ς2) => ς2 }),
          Set(),
          halted ++ todo.filter((ς) => ς.halted(kstore)),
          startingTime,
          graph.map(_.addEdges(edges)),
          kstore2,
          sem)
      }
    }
  }

  def eval(exp: Exp, sem: Semantics[Exp, Abs, Addr], graph: Boolean): Output[Abs] =
    loop(Set(new State(exp)), Set(), Set(), System.nanoTime,
      if (graph) { Some(new Graph[State]()) } else { None },
      new KontStore(), sem)
}
