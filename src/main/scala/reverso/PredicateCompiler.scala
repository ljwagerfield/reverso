package reverso

import cats.data.StateT
import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.implicits._
import reverso.PredicateAST.PredicateDefinition
import reverso.common.Extensions._
import reverso.common.UInt

class PredicateCompiler[F[_]: Concurrent] {
  private type CompilationTree[G[_[_], _], A] = StateT[G[F, *], CallStack, A]

  def compile(function: PredicateDefinition, variableLimit: UInt): F[PredicateModel[F]] =
    for {
      choco                  <- ChocoState.empty()
      pool                   <- VariablePool.empty(choco, variableLimit + 1) // + 1 for 'currentCallStack' below.
      currentCallStack       <- pool.allocateInt.assertRight
      completedCallStacksRef <- Ref.of(CompletedCallStacks.empty)
      compilation             = new Compilation(function, pool, completedCallStacksRef)
      _                      <- compilation.run()
      completedCallStacks    <- completedCallStacksRef.get
    } yield new PredicateModel[F](choco, completedCallStacks.values.toVector, currentCallStack)

  private class Compilation(
    function: PredicateDefinition,
    pool: VariablePool[F],
    completedCallStacksRef: Ref[F, CompletedCallStacks]
  ) {
    def run(): F[Unit] =
      ???
  }
}
