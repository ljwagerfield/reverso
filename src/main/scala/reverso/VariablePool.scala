package reverso

import cats.data.EitherT
import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.implicits._
import org.chocosolver.solver.Model
import reverso.VariablePool.Errors.VariableLimitReached
import reverso.Variables.{BooleanVariable, DoubleVariable, IntVariable}
import reverso.common.Extensions._
import reverso.common.{RefPessimistic, UInt}

import scala.collection.mutable

class VariablePool[F[_]: Sync](
  chocoRef: RefPessimistic[F, ChocoState],
  maxPoolSize: UInt,
  nextVariableId: Ref[F, VariableId]
) {

  /**
    * Allocates a boolean variable, if the pool has capacity.
    */
  def allocateBoolean: EitherT[F, VariableLimitReached.type, BooleanVariable] =
    allocate(
      _.booleans,
      BooleanVariable,
      (model, id) => model.boolVar(s"BooleanVariable(${id.value})")
    )

  /**
    * Allocates a boolean variable, increasing the pool's capacity if needed.
    */
  def allocateBooleanIgnoreLimit: F[BooleanVariable] =
    allocateIgnoreLimit(
      _.booleans,
      BooleanVariable,
      (model, id) => model.boolVar(s"BooleanVariable(${id.value})")
    )

  /**
    * Allocates an integer variable, if the pool has capacity.
    */
  def allocateInt(
    lowerBoundInclusive: Int,
    upperBoundInclusive: Int
  ): EitherT[F, VariableLimitReached.type, IntVariable] =
    allocate(
      _.ints,
      IntVariable,
      (model, id) => model.intVar(s"IntVariable(${id.value})", lowerBoundInclusive, upperBoundInclusive)
    )

  /**
    * Allocates an integer variable, increasing the pool's capacity if needed.
    */
  def allocateIntIgnoreLimit(lowerBoundInclusive: Int, upperBoundInclusive: Int): F[IntVariable] =
    allocateIgnoreLimit(
      _.ints,
      IntVariable,
      (model, id) => model.intVar(s"IntVariable(${id.value})", lowerBoundInclusive, upperBoundInclusive)
    )

  /**
    * Allocates a double variable, if the pool has capacity.
    */
  def allocateDouble(
    lowerBoundInclusive: Double,
    upperBoundInclusive: Double,
    precision: Double
  ): EitherT[F, VariableLimitReached.type, DoubleVariable] =
    allocate(
      _.doubles,
      DoubleVariable,
      (model, id) => model.realVar(s"DoubleVariable(${id.value})", lowerBoundInclusive, upperBoundInclusive, precision)
    )

  /**
    * Allocates a double variable, increasing the pool's capacity if needed.
    */
  def allocateDoubleIgnoreLimit(
    lowerBoundInclusive: Double,
    upperBoundInclusive: Double,
    precision: Double
  ): F[DoubleVariable] =
    allocateIgnoreLimit(
      _.doubles,
      DoubleVariable,
      (model, id) => model.realVar(s"DoubleVariable(${id.value})", lowerBoundInclusive, upperBoundInclusive, precision)
    )

  private def allocate[A, B](
    map: ChocoState => mutable.Map[A, B],
    makeExternalVariable: VariableId => A,
    allocateInternalVariable: (Model, VariableId) => B
  ): EitherT[F, VariableLimitReached.type, A] = {
    type Error = VariableLimitReached.type
    chocoRef.resource.mapK(EitherT.liftK[F, Error]).use { choco =>
      val variableCount = choco.model.getNbVars
      for {
        _        <- EitherT.cond[F](variableCount < maxPoolSize, (), VariableLimitReached)
        external <- allocateIgnoreLimit(map, makeExternalVariable, allocateInternalVariable, choco).asRightLifted[Error]
      } yield external
    }
  }

  private def allocateIgnoreLimit[A, B](
    map: ChocoState => mutable.Map[A, B],
    makeExternalVariable: VariableId => A,
    allocateInternalVariable: (Model, VariableId) => B
  ): F[A] =
    chocoRef.resource.use(
      allocateIgnoreLimit(map, makeExternalVariable, allocateInternalVariable, _)
    )

  private def allocateIgnoreLimit[A, B](
    map: ChocoState => mutable.Map[A, B],
    makeExternalVariable: VariableId => A,
    allocateInternalVariable: (Model, VariableId) => B,
    choco: ChocoState
  ): F[A] =
    for {
      variableId <- getAndIncVariableId
      internal    = allocateInternalVariable(choco.model, variableId)
      external    = makeExternalVariable(variableId)
      _           = map(choco).update(external, internal)
    } yield external

  private def getAndIncVariableId: F[VariableId] =
    nextVariableId.modify(current => VariableId(current.value + 1) -> current)
}

object VariablePool {
  def empty[F[_]: Concurrent](choco: RefPessimistic[F, ChocoState], maxPoolSize: UInt): F[VariablePool[F]] =
    Ref.of(VariableId(0)).map(initialVariableId => new VariablePool[F](choco, maxPoolSize, initialVariableId))

  object Errors {
    case object VariableLimitReached
  }
}
