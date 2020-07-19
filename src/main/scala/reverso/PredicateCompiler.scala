package reverso

import cats.Applicative
import cats.data.{OptionT, StateT}
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.implicits._
import reverso.PredicateAST.Terminal.{Continue, Success}
import reverso.PredicateAST.{Constraint, PredicateDefinition, Terminal}
import reverso.common.Extensions._
import reverso.common.MapK.{fToListT => fromF, optionTtoListT => fromOption}
import reverso.common.{ListT, UInt}

class PredicateCompiler[F[_]: Concurrent] {
  private type CompilationTreeT[G[_[_], _], A] = StateT[G[F, *], CallStack, A]
  private type CompilationTree[A]              = StateT[F, CallStack, A]

  /**
    * Compiles a predicate definition into a predicate model, which you can then use (and re-use)
    * to generate valid inputs for your predicate definition.
    *
    * @param predicate     Predicate definition.
    * @param variableLimit The maximum number of variables that can be allocated on the heap by your predicate.
    *                      Larger values will allow the model to produce larger (and thus more permutations of) inputs.
    *
    * @return              Predicate model: use this to generate inputs that satisfy your predicate.
    */
  def compile(predicate: PredicateDefinition, variableLimit: UInt): F[PredicateModel[F]] =
    for {
      choco                  <- ChocoState.empty()
      pool                   <- VariablePool.empty(choco, variableLimit + 1) // + 1 for 'currentCallStack' below.
      currentCallStack       <- pool.allocateInt.assertRight
      completedCallStacksRef <- Ref.of(CompletedCallStacks.empty)
      compilation             = new Compilation(predicate, pool, completedCallStacksRef)
      _                      <- compilation.run()
      completedCallStacks    <- completedCallStacksRef.get
    } yield new PredicateModel[F](choco, completedCallStacks.values.toVector, currentCallStack)

  /**
    * Compilation job with Choco model initialized.
    */
  private class Compilation(
    function: PredicateDefinition,
    pool: VariablePool[F],
    completedCallStacksRef: Ref[F, CompletedCallStacks]
  ) {
    def run(): F[Unit] = {
      val compilationTree = processStatementsUntilPoolSaturated()
      val wipCallStacks   = compilationTree.run(CallStack.empty)
      wipCallStacks.value.void // Discard WIP call-stacks: the completed ones will have been added to 'Ref'.
    }

    private def processStatementsUntilPoolSaturated(): CompilationTreeT[ListT, Unit] = {
      val (continue, success) =
        function.body.statements.toList.partitionEither {
          case (predicates, terminal @ Success) => (predicates -> terminal).asRight
          case (predicates, terminal: Continue) => (predicates -> terminal).asLeft
        }

      for {
        _ <- processStatements(success)
        _ <- ().tailRecM(_ => processStatements(continue).map(_.asLeft[Unit])) // Ends when variable pool saturates.
      } yield ()
    }

    private def processStatements(statements: List[(List[Constraint], Terminal)]): CompilationTreeT[ListT, Unit] =
      for {
        _ <- addStatements(statements)
        _ <- filterConsistentStatements.mapK(fromOption)
        _ <- yieldIfAtInitialState.mapK(fromF)
      } yield ()

    private def addStatements(statements: List[(List[Constraint], Terminal)]): CompilationTreeT[ListT, Unit] =
      for {
        // Todo: update current 'CallStack' to include new stack frame (after 'statement' line below).
        statement              <- CompilationTreeT.fan(statements)
        (constraints, terminal) = statement
        _                      <- addConstraints(constraints).mapK(fromOption)
        _                      <- addTerminal(terminal).mapK(fromOption)
      } yield ()

    private def filterConsistentStatements: CompilationTreeT[OptionT, Unit] = {}

    private def yieldIfAtInitialState: CompilationTree[Unit] =
      CompilationTree.get.flatMapF { callStack =>
        if (isInitialState(callStack))
          yieldInitialState(callStack)
        else
          ().pure[F]
      }

    private def isInitialState(callStack: CallStack): Boolean = {}

    private def yieldInitialState(callStack: CallStack): F[Unit] =
      completedCallStacksRef.update(_.add(callStack))

    private def addConstraints(constraints: List[Constraint]): CompilationTreeT[OptionT, Unit] =
      constraints.traverse_(addConstraint)

    private def addConstraint(constraint: Constraint): CompilationTreeT[OptionT, Unit] = {
      // Return NONE when variables exceed graph.maxVariables
    }

    private def addTerminal(terminal: Terminal): CompilationTreeT[OptionT, Unit] =
      terminal match {
        case Success            => ().pure[CompilationTreeT[OptionT, *]]
        case continue: Continue => ??? // Return NONE when variables exceed graph.maxVariables
      }
  }

  /**
    * Compilation Tree Helper (inspired by State companion object).
    */
  private object CompilationTree {
    def get: CompilationTree[CallStack] =
      StateT.get[F, CallStack]
  }

  /**
    * Compilation Tree Helper (inspired by StateT companion object).
    */
  private object CompilationTreeT {
    def apply[G[_[_], _], A](
      fa: CallStack => G[F, (CallStack, A)]
    )(implicit A: Applicative[G[F, *]]): CompilationTreeT[G, A] =
      StateT.apply[G[F, *], CallStack, A](fa)

    def fan[A](children: List[A]): CompilationTreeT[ListT, A] =
      CompilationTreeT { callStack =>
        ListT.fromList(
          children.map(callStack -> _)
        )
      }

  }
}
