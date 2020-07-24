package reverso.common

import cats.effect.{Concurrent, Resource, Sync}
import cats.effect.concurrent.{MVar, MVar2}
import cats.implicits._

/**
  * Provides mutexed (i.e. serial) access to a resource.
  *
  * Differences between 'Ref' and 'RefPessimistic':
  *
  * 1.  'Ref' replays updates. 'RefPessimistic' does not: all thunks are invoked exactly once.
  * 2.  'Ref' allows parallel reads. 'RefPessimistic' enforces serial reads.
  *
  * Useful for: wrapping mutable objects (e.g. from third-party APIs).
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
