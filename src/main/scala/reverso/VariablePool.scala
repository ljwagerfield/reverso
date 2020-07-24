package reverso

import cats.data.{EitherT, OptionT}
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import org.chocosolver.solver.Model
import reverso.VariablePool.VariablePoolError
import reverso.VariablePool.VariablePoolError.{ConsumerCounterIsAheadOfCentralCounter, VariableLimitReached}
import reverso.VariableRef.{BooleanVariable, DoubleVariable, IntVariable}
import reverso.common.Extensions._
import reverso.common.{RefPessimistic, UInt}

import scala.collection.mutable

/**
  * An object pool that issues the same sequence of objects to each consumer.
  *
  * This allows stack frames across different call stacks to share the same set of variables (through half-reification).
  *
  * This provides O(logN) space complexity compared to new variables being allocated on each request. This is an
  * important optimisation: consider a predicate where each stack frame allocates exactly one variable, and where each
  * stack frame has 10 possible parent stack frames, and the compiler is left to traverse the predicate up to a call
  * stack depth of 10 frames. Without variable reuse we would need 10bn variables (i.e. the tree size). With variable
  * reuse we only need 10 (i.e. the tree depth).
  *
  * How it works:
  *
  * Consumers create a variable reference (an integer) and then assign a variable to it by calling issue* on the pool.
  * The consumer then increments their reference, such that on their next call, they will be assigning a variable to a
  * different reference. If the pool has already allocated a variable for the given reference before, it does nothing,
  * else it allocates a new variable for the reference in the internal Choco model. Since a consumer always increments
  * their reference after each allocation, they will never request the same variable twice, but other consumers (i.e.
  * call stacks) are guaranteed to consume the same sequence of variables.
  */
class VariablePool[F[_]: Sync](
  chocoRef: RefPessimistic[F, ChocoState],
  partitionLimits: Map[VariablePoolPartition, UInt],
  allocationCounter: Ref[F, VariableCounter]
) {

  def issueBoolean(boolean: BooleanVariable): EitherT[F, VariablePoolError, Unit] =
    issueVariable(boolean, _.booleans) { (model, id) =>
      model.boolVar(s"${id.definition.partition}.Booleans.${id.index}")
    }

  def issueInt(int: IntVariable): EitherT[F, VariablePoolError, Unit] =
    issueVariable(int, _.ints) { (model, id) =>
      model.intVar(
        s"${id.definition.partition}.Ints.${id.index}",
        int.definition.variableType.lowerBound,
        int.definition.variableType.upperBound
      )
    }

  def issueDouble(double: DoubleVariable): EitherT[F, VariablePoolError, Unit] =
    issueVariable(double, _.doubles) { (model, id) =>
      model.realVar(
        s"${id.definition.partition}.Doubles.${id.index}",
        double.definition.variableType.lowerBound,
        double.definition.variableType.upperBound,
        double.definition.variableType.precision
      )
    }

  /**
    * Allocates the variable if it does not yet exist in the pool, else does nothing.
    */
  private def issueVariable[A <: VariableType, B](
    variableRef: VariableRef[A],
    map: ChocoState => mutable.Map[VariableRef[A], B]
  )(
    allocateChocoVariable: (Model, VariableRef[A]) => B
  ): EitherT[F, VariablePoolError, Unit] =
    EitherT(
      chocoRef.resource.use { choco =>
        allocationCounter
          .tryModify { oldCounter =>
            val newCounterMaybe =
              for {
                newCounter <- incrementAllocationCount(oldCounter, variableRef)
                _          <- allocate(variableRef, choco, map, allocateChocoVariable).someLifted
              } yield newCounter

            val newCounterOpt = newCounterMaybe.value.getOrElse(None)
            val newCounter    = newCounterOpt.getOrElse(oldCounter)

            newCounter -> newCounterMaybe.value.void
          }
          // We use 'Ref' here just to make our code's intentions more explicit: in reality, it's safe to mutate a
          // global 'var' here since an outer mutex is already present on Choco. Consequently, we don't expect the inner
          // 'Ref' to ever experience contention, so we use 'tryModify' to make that expectation explicit. Further, if
          // there were any contention on the inner Ref, it's not OK to allow it, as we can't replay mutations to the
          // Choco model, so we throw an exception in this unexpected scenario instead of replaying the mutation.
          .map(_.getOrElse(throw new Exception("Unexpected contention on ref: the outer mutex should prevent this.")))
      }
    )

  /**
    * Increments the allocation counter if required (returns None if no increment is required).
    */
  private def incrementAllocationCount[A <: VariableType](
    oldCounter: VariableCounter,
    variableRef: VariableRef[A]
  ): OptionT[Either[VariablePoolError, *], VariableCounter] =
    OptionT {
      val (incrementedCounter, nextVariableRef) = oldCounter.allocateVariable(variableRef.definition)
      val requestWithinBounds                   = variableRef.index <= nextVariableRef.index
      val allocationRequested                   = variableRef.index === nextVariableRef.index
      Either.cond(
        requestWithinBounds,
        Option.when(allocationRequested)(incrementedCounter),
        ConsumerCounterIsAheadOfCentralCounter
      )
    }

  /**
    * Creates a new Choco variable, and then adds it to ChocoState.
    */
  private def allocate[A, B](
    variableRef: VariableRef[A],
    choco: ChocoState,
    map: ChocoState => mutable.Map[VariableRef[A], B],
    allocateChocoVariable: (Model, VariableRef[A]) => B
  ): Either[VariablePoolError, Unit] = {
    val variableCount      = choco.model.getNbVars
    val variableCountLimit = partitionLimits.get(variableRef.definition.partition)
    val cond               = Either.cond(variableCountLimit.forall(variableCount < _), (), VariableLimitReached)

    // Side-effecting code. This is OK this method is called inside a mutex AND is never replayed.
    cond.foreach { _ =>
      map(choco).update(
        variableRef,
        allocateChocoVariable(choco.model, variableRef)
      )
    }

    cond
  }

}

object VariablePool {
  def empty[F[_]: Concurrent](
    choco: RefPessimistic[F, ChocoState],
    maxPoolSize: Map[VariablePoolPartition, UInt]
  ): F[VariablePool[F]] =
    Ref.of(VariableCounter.empty).map(initialVariableId => new VariablePool[F](choco, maxPoolSize, initialVariableId))

  sealed trait VariablePoolError extends Product

  object VariablePoolError {
    case object VariableLimitReached                   extends VariablePoolError
    case object ConsumerCounterIsAheadOfCentralCounter extends VariablePoolError
  }
}
