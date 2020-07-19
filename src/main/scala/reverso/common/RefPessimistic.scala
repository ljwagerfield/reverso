package reverso.common

import cats.effect.{Concurrent, Resource, Sync}
import cats.effect.concurrent.{MVar, MVar2}
import cats.implicits._

/**
  * Pessimistic (and cut-down) version of Ref.
  *
  * Useful for wrapping mutable objects from third-party APIs that cannot have 'modify' replayed to them multiple times.
  *
  * See: https://typelevel.org/cats-effect/concurrency/mvar.html#use-case-asynchronous-lock-binary-semaphore-mutex
  */
final class RefPessimistic[F[_]: Sync, A](mvar: MVar2[F, A]) {

  /**
    * Resource that can only be 'used' once-at-a-time.
    */
  def resource: Resource[F, A] =
    Resource.make(acquire)(release)

  /**
    * Provides access to the [[resource]] via a synchronous accessor.
    */
  def accessSync[B](fa: A => B): F[B] =
    Sync[F].bracket(acquire)(a => fa(a).pure[F])(release)

  private def acquire: F[A] =
    mvar.take

  private def release(value: A): F[Unit] =
    mvar.put(value)
}

object RefPessimistic {
  def of[F[_]: Concurrent, A](value: A): F[RefPessimistic[F, A]] =
    MVar.of[F, A](value).map(new RefPessimistic[F, A](_))
}
