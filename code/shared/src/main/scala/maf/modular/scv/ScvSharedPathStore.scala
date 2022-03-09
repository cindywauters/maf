package maf.modular.scv

/**
 * Shares the entire path store (after restoring renames of symbolic variables) across function call components.
 *
 * It is implemented by writing all the path conditions (a disjunction of conjunctions) to the global store. Then this path condition is used by the
 * caller to generate successor states.
 */
trait ScvSharedPathStore extends maf.modular.scv.BaseScvBigStepSemantics with ScvSymbolicStore.GlobalSymbolicStore:
    override def intraAnalysis(component: Component): SharedPathStoreIntra

    trait SharedPathStoreIntra extends BaseIntraScvSemantics with GlobalMapStoreIntra:
        import scvMonadInstance.*
        import maf.core.Monad.MonadSyntaxOps

        private def readPathCondition(targetCmp: Component): List[PathStore] =
          readMapAddr(targetCmp)

        override protected def runIntraSemantics(initialState: State): Set[(PostValue, PathStore)] =
            val answers = super.runIntraSemantics(initialState)
            writeMapAddr(cmp, answers.map(_._2).toList)
            answers

        override protected def afterCall(targetCmp: Component): EvalM[Unit] =
          // this is a very crude approximation, we propebably don't need the entire path condition from the target
          context(targetCmp) match
              case Some(KPathCondition(_, _, _, _, changes)) =>
                val pss = readPathCondition(targetCmp)
                val updatedPss = pss.map(_.revertChanges(changes))
                // Combine path store with current path store, branch if nessary
                nondets(updatedPss.map { ps =>
                  for
                      pc <- getPc
                      // TODO: remove parts of path condition that are about variables in our current scope
                      _ <- putPc(ps.pc ++ pc)
                  yield ()
                }.toSet)
              case _ => unit(())
