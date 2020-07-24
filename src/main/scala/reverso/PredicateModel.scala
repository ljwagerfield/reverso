package reverso

import cats.effect.{Concurrent, Resource}
import cats.implicits._
import fs2.Stream
import reverso.SolvedValue.SolvedObject
import reverso.VariableRef.IntVariable
import reverso.common.RefPessimistic

/**
  * Provides access to the predicate's solutions.
  *
  * Thread-safe: yes.
  */
class PredicateModel[F[_]: Concurrent](
  chocoRef: RefPessimistic[F, ChocoState],
  validCallStacks: Vector[CallStack],
  callStackIndex: IntVariable
) {

  /**
    * Warning: Deadlock will occur if the thunk passed into 'solutions.use' calls other methods that use [[chocoRef]].
    *          Please collect the results you need by returning them from the 'solutions.use' thunk, and make subsequent
    *          calls outside of the thunk if necessary.
    */
  def solutions: Resource[F, Stream[F, SolvedObject]] =
    chocoRef.resourceWithSolver.evalMap {
      case (choco, solver) =>
        RefPessimistic.of[F, Unit](()).map { serialConsumer =>
          Stream
            .repeatEval {
              // Even though we're guaranteed no-one else is concurrently using the 'chocoRef' resource, we still cannot
              // guarantee the consumer of the stream that currently has access to 'chocoRef' is single-threaded. If
              // it's multi-threaded, the thunk we pass into 'repeatEval' will be executed multiple times in parallel.
              // Therefore, we wrap the eval in a mutex, ensuring single-threaded access to the model while 'solve' is
              // executed and snapshots of the variables are taken for the solution.
              serialConsumer.accessSync { _ =>
                Option.when(solver.solve()) {
                  // Todo: Traverse 'pointers' objects below, and construct 'SolvedObject' from it, resolving the
                  //   *Variables in the 'pointers' objects to *Vars in the 'choco' instance.
                  val callStack                        = validCallStacks(choco.ints(callStackIndex).getValue)
                  val pointers                         = callStack.pointers
                  val structuredSolution: SolvedObject = ???
                  structuredSolution
                }
              }
            }
            .collectWhile { case Some(solution) => solution }
        }
    }

}
