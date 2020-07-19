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
  def size: F[Int] = chocoRef.accessSync(_.model.getNbVars)

  def allocateBoolean: EitherT[F, VariableLimitReached.type, BooleanVariable] =
    allocate(
      _.booleans,
      BooleanVariable,
      (model, id) => model.boolVar(s"BooleanVariable(${id.value})")
    )

  def allocateInt: EitherT[F, VariableLimitReached.type, IntVariable] =
    allocate(
      _.ints,
      IntVariable,
      (model, id) => model.intVar(s"IntVariable(${id.value})", -10000000, 10000000)
    )

  def allocateDouble: EitherT[F, VariableLimitReached.type, DoubleVariable] =
    allocate(
      _.doubles,
      DoubleVariable,
      (model, id) => model.realVar(s"DoubleVariable(${id.value})", -100D, 100D, 0.00001D)
    )

  private def allocate[A, B](
    map: ChocoState => mutable.Map[A, B],
    makeExternalVariable: VariableId => A,
    allocateInternalVariable: (Model, VariableId) => B
  ): EitherT[F, VariableLimitReached.type, A] = {
    type Error = VariableLimitReached.type
    chocoRef.resource.mapK(EitherT.liftK[F, Error]).use { choco =>
      for {
        variableCount <- size.asRightLifted[Error]
        _             <- EitherT.cond[F](variableCount < maxPoolSize, (), VariableLimitReached)
        variableId    <- getAndIncVariableId.asRightLifted[Error]
        internal       = allocateInternalVariable(choco.model, variableId)
        external       = makeExternalVariable(variableId)
        _              = map(choco).update(external, internal)
      } yield external
    }
  }

  private def getAndIncVariableId: F[VariableId] =
    nextVariableId.modify(current => VariableId(current.value + 1) -> current)
}

object VariablePool {
  def empty[F[_]: Concurrent](choco: RefPessimistic[F, ChocoState], maxPoolSize: UInt): F[VariablePool[F]] =
    for {
      nextVariableId <- Ref.of(VariableId(0))
    } yield new VariablePool[F](choco, maxPoolSize, nextVariableId)

  object Errors {
    case object VariableLimitReached
  }
}
