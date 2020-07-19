package reverso

import cats.effect.Concurrent
import cats.effect.concurrent.Ref
import cats.implicits._
import reverso.FunctionAST.FunctionDefinition
import reverso.common.Extensions._
import reverso.common.UInt

class FunctionCompiler[F[_]: Concurrent] {
  def compile(function: FunctionDefinition, variableLimit: UInt): F[FunctionModel[F]] =
    for {
      choco                  <- ChocoState.empty()
      pool                   <- VariablePool.empty(choco, variableLimit + 1) // + 1 for 'currentCallStack' below.
      currentCallStack       <- pool.allocateInt.assertRight
      completedCallStacksRef <- Ref.of(CompletedCallStacks.empty)
      compilation             = new Compilation(function, pool, completedCallStacksRef)
      _                      <- compilation.run()
      completedCallStacks    <- completedCallStacksRef.get
    } yield new FunctionModel[F](choco, completedCallStacks.values.toVector, currentCallStack)

  private class Compilation(
    function: FunctionDefinition,
    pool: VariablePool[F],
    completedCallStacksRef: Ref[F, CompletedCallStacks]
  ) {
    def run(): F[Unit] =
      ???
  }
}
