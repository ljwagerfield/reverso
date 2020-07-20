package reverso

import cats.Applicative
import cats.data.{EitherT, NonEmptyList, OptionT, StateT}
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.implicits._
import reverso.PredicateAST.Terminal.{Continue, Success}
import reverso.PredicateAST.{Constraint, PredicateDefinition, Terminal}
import reverso.PredicateCompiler.Errors.NoSolutionsFound
import reverso.Variables.IntVariable
import reverso.common.Extensions._
import reverso.common.MapK.{fToListT => fromF, optionTtoListT => fromOption}
import reverso.common.{ListT, RefPessimistic, UInt}

class PredicateCompiler[F[_]: Concurrent] {
  private type CompilationTreeT[G[_[_], _], A] = StateT[G[F, *], CallStack, A]
  private type CompilationTree[A]              = StateT[F, CallStack, A]

  /**
    * Compiles a predicate into a model, which you can then use (and re-use) to generate inputs for your predicate.
    * @param predicate     Predicate definition.
    * @param variableLimit The maximum number of variables that can be allocated on the heap by your predicate.
    *                      Higher values allow the model to produce larger (and thus more permutations of) inputs.
    * @return              Predicate model that can be used to generate inputs that satisfy your predicate.
    */
  def compile(
    predicate: PredicateDefinition,
    variableLimit: UInt
  ): EitherT[F, NoSolutionsFound.type, PredicateModel[F]] = {
    type Error = NoSolutionsFound.type
    for {
      compilerContext  <- newCompilerContext(predicate, variableLimit).asRightLifted[Error]
      _                <- compilerContext.generateValidCallStacks().asRightLifted[Error]
      validCallStacks  <- compilerContext.getValidCallStacks
      currentCallStack <- compilerContext.allocateCurrentStackIndexVariable(validCallStacks.size).asRightLifted[Error]
    } yield new PredicateModel[F](compilerContext.choco, validCallStacks.toList.toVector, currentCallStack)
  }

  /**
    * Instantiates shared variables required by the various private compiler methods.
    */
  private def newCompilerContext(
    predicate: PredicateDefinition,
    variableLimit: UInt
  ): F[CompilerContext] =
    for {
      choco              <- ChocoState.empty()
      pool               <- VariablePool.empty(choco, variableLimit + 1) // + 1 for 'currentCallStack' below.
      validCallStacksRef <- Ref.of(ValidCallStacks.empty)
      compilation         = new CompilerContext(choco, pool, predicate, validCallStacksRef)
    } yield compilation

  /**
    * Private methods grouped into a class to provide easier access to shared variables.
    */
  private class CompilerContext(
    val choco: RefPessimistic[F, ChocoState],
    pool: VariablePool[F],
    predicate: PredicateDefinition,
    validCallStacksRef: Ref[F, ValidCallStacks]
  ) {

    /**
      * Saves results to [[validCallStacksRef]].
      */
    def generateValidCallStacks(): F[Unit] = {
      val compilationTree = processStatementsUntilPoolSaturated()
      val wipCallStacks   = compilationTree.run(CallStack.empty)
      wipCallStacks.value.void
    }

    /**
      * Returns results saved by [[generateValidCallStacks()]].
      */
    def getValidCallStacks: EitherT[F, NoSolutionsFound.type, NonEmptyList[CallStack]] =
      EitherT(
        for {
          validCallStacks   <- validCallStacksRef.get
          validCallStacksNel = validCallStacks.values.toNel
        } yield validCallStacksNel.toRight(NoSolutionsFound)
      )

    def allocateCurrentStackIndexVariable(validCallStackCount: Int): F[IntVariable] =
      pool.allocateIntIgnoreLimit(
        lowerBoundInclusive = 0,
        upperBoundInclusive = validCallStackCount - 1
      )

    private def processStatementsUntilPoolSaturated(): CompilationTreeT[ListT, Unit] = {
      val (continue, success) =
        predicate.body.statements.toList.partitionEither {
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
      validCallStacksRef.update(_.add(callStack))

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
    * Helper inspired by State companion object.
    */
  private object CompilationTree {
    def get: CompilationTree[CallStack] =
      StateT.get[F, CallStack]
  }

  /**
    * Helper inspired by StateT companion object.
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

object PredicateCompiler {
  object Errors {

    /**
      * No solutions could be found for the predicate: either there is no solution, or a solution requires more
      * variables (in which case the variable limit must be raised).
      */
    case object NoSolutionsFound
  }
}
