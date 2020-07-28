package reverso

import cats.Applicative
import cats.data.{EitherT, NonEmptyList, OptionT, StateT}
import cats.effect.{Concurrent, Resource, Sync}
import cats.effect.concurrent.Ref
import cats.implicits._
import org.chocosolver.solver.Solver
import org.chocosolver.solver.constraints.extension.Tuples
import reverso.PredicateAST.Terminal.{Continue, Success}
import reverso.PredicateAST.{Assignment, Constraint, PredicateDefinition, Terminal}
import reverso.PredicateCompiler.Errors.NoSolutionsFound
import reverso.UndefinedPointerGraph.FieldValue.{Defined, DefinedAs, Undefined}
import reverso.VariablePoolPartition.{MetaVariables, StackFrameVariables}
import reverso.VariableRef.IntVariable
import reverso.VariableType.IntVariableType
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
      compilerContext <- newCompilerContext(predicate, variableLimit).asRightLifted[Error]
      _               <- compilerContext.generateValidCallStacks().asRightLifted[Error]
      validCallStacks <- compilerContext.getValidCallStacks
      callStackIndex  <- compilerContext.allocateCallStackIndex(validCallStacks.size).asRightLifted[Error]
      _               <- compilerContext.limitOneCallStackPerSolution(validCallStacks, callStackIndex).asRightLifted[Error]
    } yield new PredicateModel[F](compilerContext.chocoRef, validCallStacks.toList.toVector, callStackIndex)
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
      pool               <- VariablePool.empty(choco, Map(StackFrameVariables -> variableLimit))
      validCallStacksRef <- Ref.of(ValidCallStacks.empty)
      compilation         = new CompilerContext(choco, pool, predicate, validCallStacksRef)
    } yield compilation

  /**
    * Private methods grouped into a class to provide easier access to shared variables.
    */
  private class CompilerContext(
    val chocoRef: RefPessimistic[F, ChocoState],
    pool: VariablePool[F],
    predicate: PredicateDefinition,
    validCallStacksRef: Ref[F, ValidCallStacks]
  ) {

    /**
      * Saves results to [[validCallStacksRef]].
      */
    def generateValidCallStacks(): F[Unit] = {
      // Todo: Remove all constraints and variables added by WIP call stacks that never became valid call stacks.
      //       Do this by calling 'removeStackFramesFromModel(unusedStackFrames)'. Calculate 'unusedStackFrames' first!
      val compilationTree = fanOutCallStacksUntilPoolSaturated()
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

    def allocateCallStackIndex(validCallStackCount: Int): F[IntVariable] = {
      val callStackIndexBounds = IntVariableType(0, validCallStackCount - 1)
      val callStackIndex       = VariableRef(VariableDefinition(MetaVariables, callStackIndexBounds), 0)
      pool
        .issueInt(callStackIndex)
        .assertRight
        .as(callStackIndex)
    }

    private def fanOutCallStacksUntilPoolSaturated(): CompilationTreeT[ListT, Unit] = {
      val (continue, success) =
        predicate.body.statements.toList.partitionEither {
          case (predicates, terminal @ Success) => (predicates -> terminal).asRight
          case (predicates, terminal: Continue) => (predicates -> terminal).asLeft
        }
      for {
        _ <- fanOutCallStacks(success)
        _ <- ().tailRecM(_ => fanOutCallStacks(continue).map(_.asLeft[Unit])) // Ends when variable pool saturates.
      } yield ()
    }

    private def fanOutCallStacks(stackFrames: List[(List[Constraint], Terminal)]): CompilationTreeT[ListT, Unit] =
      for {
        _ <- addParentStackFrames(stackFrames)
        _ <- removeInconsistentCallStacks.mapK(fromOption)
        _ <- yieldIfCallStackIsFinished.mapK(fromF)
      } yield ()

    private def addParentStackFrames(stackFrames: List[(List[Constraint], Terminal)]): CompilationTreeT[ListT, Unit] =
      for {
        // Todo: update current CallStack (in StateT) to include new stack frame (after 'fan' line below).
        stackFrame             <- CompilationTreeT.fan(stackFrames)
        (constraints, terminal) = stackFrame
        _                      <- addConstraints(constraints).mapK(fromOption)
        _                      <- addTerminal(terminal).mapK(fromOption)
      } yield ()

    private def addConstraints(constraints: List[Constraint]): CompilationTreeT[OptionT, Unit] =
      constraints.traverse_(addConstraint)

    private def addTerminal(terminal: Terminal): CompilationTreeT[OptionT, Unit] =
      terminal match {
        case Success               => ().pure[CompilationTreeT[OptionT, *]]
        case Continue(assignments) => addAssignments(assignments)
      }

    private def addAssignments(assignments: List[Assignment]): CompilationTreeT[OptionT, Unit] =
      assignments.traverse_(addAssignment)

    private def addConstraint(constraint: Constraint): CompilationTreeT[OptionT, Unit] = {
      // Return NONE when variables exceed graph.maxVariables
      // Aim of this method (and the 'addTerminal' method) is to:
      // - Identify which variables in this stack frame reference new variables, or reference existing variables.
      // - If existing: get said variable.
      // - Else if new: allocate and record its structure in 'PointerGraph'
      // - If the constraint is 'Relational': record the constraints in Choco.
    }

    private def addAssignment(assignment: Assignment): CompilationTreeT[OptionT, Unit] = {
      // Return NONE when variables exceed graph.maxVariables
      // Aim of this method (and the 'addTerminal' method) is to:
      // - Identify which variables in this stack frame reference new variables, or reference existing variables.
      // - If existing: get said variable.
      // - Else if new: allocate and record its structure in 'PointerGraph'
      // - If the constraint is 'Relational': record the constraints in Choco.
    }

    private def removeInconsistentCallStacks: CompilationTreeT[OptionT, Unit] =
      CompilationTreeT(callStack =>
        OptionT.whenF(
          for {
            isConsistent <- isCallStackConsistent(callStack)
            _            <- if (!isConsistent) removeStackFramesFromModel(callStack.frames.take(1)) else ().pure[F]
          } yield isConsistent
        )(callStack -> ())
      )

    private def isCallStackConsistent(callStack: CallStack): F[Boolean] =
      limitOneCallStackPerSolutionTemp(callStack).use(_.solve().pure[F])

    /**
      * Constrain the reification variables used to activate stack frames, such that stack frames can only be enabled in
      * combinations that represent valid call stacks. Also zeros all variables not in use by each call stack (preventing
      * the solver from wasting time permeating an unused set of variables).
      */
    def limitOneCallStackPerSolution(callStacks: NonEmptyList[CallStack], callStackIndex: IntVariable): F[Unit] =
      limitOneCallStackPerSolution(callStacks, Some(callStackIndex), keepConstraints = true).use(_.pure[F].void)

    private def limitOneCallStackPerSolutionTemp(callStack: CallStack): Resource[F, Solver] =
      limitOneCallStackPerSolution(NonEmptyList.one(callStack), None, keepConstraints = false)

    // Zero variables by adding them to table as 0s for each variable that wasn't acquired, and STAR for those that were.
    // Requires us to maintain an exclusive pool of variables per call stack.
    private def limitOneCallStackPerSolution(
      callStacks: NonEmptyList[CallStack],
      callStackIndexVariable: Option[IntVariable],
      keepConstraints: Boolean
    ): Resource[F, Solver] =
      for {
        chocoWithSolver <- chocoRef.resourceWithSolver
        (choco, solver)  = chocoWithSolver
        variableMasks    = VariableMaskSet.from(choco, callStacks, callStackIndexVariable)
        constraints      = variableMasks.toConstraints(choco)
        addConstraint    = Sync[F].delay(choco.model.post(constraints: _*))
        removeConstraint = Sync[F].delay(choco.model.unpost(constraints: _*))
        _               <- Resource.make(addConstraint)(_ => if (keepConstraints) ().pure[F] else removeConstraint)
      } yield solver

    private def removeStackFramesFromModel(stackFrames: List[StackFrame]): F[Unit] =
      chocoRef.accessSync { choco =>
        // Todo: Remove all variables and constraints introduced by each of these stack frames.
      }

    private def yieldIfCallStackIsFinished: CompilationTree[Unit] =
      CompilationTree.get.flatMapF { callStack =>
        if (isCallStackComplete(callStack))
          yieldValidCallStack(callStack)
        else
          ().pure[F]
      }

    private def isCallStackComplete(callStack: CallStack): Boolean = {
      val inputs             = predicate.signature.inputs
      val undefinedValues    = callStack.pointers.root.fields
      val undefinedRegisters = undefinedValues.filterNot { case (field, _) => inputs.contains(field) }.values
      val allRegistersAreExpectedToBeUndefined =
        undefinedRegisters.forall {
          case Undefined              => true
          case Defined | _: DefinedAs => false
        }
      allRegistersAreExpectedToBeUndefined
    }

    private def yieldValidCallStack(callStack: CallStack): F[Unit] =
      validCallStacksRef.update(_.add(callStack))

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
