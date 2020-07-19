package reverso

import cats.effect.{Concurrent, Resource, Sync}
import cats.implicits._
import org.chocosolver.solver.{Model, Solver}
import org.chocosolver.solver.variables.{BoolVar, IntVar, RealVar}
import reverso.Variables.{BooleanVariable, DoubleVariable, IntVariable}
import reverso.common.RefPessimistic

import scala.collection.mutable

/**
  * Choco model with its associated 'variable tokens'.
  *
  * Variable tokens are given out instead of Choco's *Var types, as access to these objects needs to be marshalled via a
  * mutex (the *Var types provide access to the model, which is mutable). Variable tokens allow our code to reference
  * Choco variables in a pure way, without having to worry about concurrency or locking.
  *
  * Why we don't use 'Ref' (not even for the 'Map' objects):
  *
  *   Pessimistic locking is required for [[Model]] because it's a mutable object, meaning it cannot have concurrent
  *   readers or mutations replayed to it. This makes 'Ref' unsuitable, so we use [[RefPessimistic]] instead. Now, since
  *   the 'Map' objects expose the [[Model]] (the *Var types have a public accessor for the model), we want to ensure
  *   access to the 'Map' objects use the same lock as [[Model]]. Thus, the entire [[ChocoState]] instance is wrapped in
  *   a [[RefPessimistic]], providing pessimistic concurrency control for all the objects in the class as a single
  *   atomic unit. To wrap additional 'Ref' objects around the 'Map' objects would be redundant: they would never enter
  *   contention.
  *
  * Thread-safe access is provided via [[RefPessimistic]].
  */
class ChocoState(
  val model: Model,
  val booleans: mutable.Map[BooleanVariable, BoolVar],
  val ints: mutable.Map[IntVariable, IntVar],
  val doubles: mutable.Map[DoubleVariable, RealVar]
)

object ChocoState {

  def empty[F[_]: Concurrent](): F[RefPessimistic[F, ChocoState]] =
    RefPessimistic.of(new ChocoState(new Model(), mutable.Map.empty, mutable.Map.empty, mutable.Map.empty))

  implicit class RichChocoState[F[_]](val chocoRef: RefPessimistic[F, ChocoState]) extends AnyVal {

    /**
      * Ensures 'solver' is cleaned up after use.
      */
    def resourceWithSolver(implicit S: Sync[F]): Resource[F, (ChocoState, Solver)] =
      for {
        choco  <- chocoRef.resource
        solver <- Resource.make(choco.model.getSolver.pure[F])(_.hardReset().pure[F]) // Todo: will 'reset' work here?
      } yield choco -> solver

  }
}
