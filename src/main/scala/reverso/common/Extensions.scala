package reverso.common

import cats.data.{EitherT, OptionT}
import cats.implicits._
import cats.{Functor, MonadError}

object Extensions {
  implicit class RichF[F[_], A](val value: F[A]) extends AnyVal {
    def asRightLifted[E](implicit functor: Functor[F]): EitherT[F, E, A] =
      EitherT.right[E][F, A](value)

    def asLeftLifted[E](implicit functor: Functor[F]): EitherT[F, A, E] =
      EitherT.left[E][F, A](value)
  }

  implicit class RichEitherT[F[_], A, B](val value: EitherT[F, A, B]) {

    /**
      * Fails the [[F]] on [[Left]].
      *
      * When faced with an error, our convention is to return it as a type disjunction if the caller is at fault OR has
      * some recourse, else we raise it as an exception: signalling the caller did everything right, but something went
      * wrong and no-one up the call stack will be able to handle it -- beyond generic error handling (and being on the
      * JVM, all generic error handlers should handle exceptions).
      */
    def assertRight(implicit me: MonadError[F, Throwable]): F[B] =
      value.value.flatMap {
        case Left(e)  => new Exception(s"Unexpected error: expected Right(...) but got Left($e)").raiseError[F, B]
        case Right(r) => r.pure[F]
      }
  }

  implicit class RichOptionTCompanion(val value: OptionT.type) extends AnyVal {
    def whenF[F[_]: Functor, A](cond: F[Boolean])(a: => A): OptionT[F, A] =
      OptionT(cond.map(Option.when(_)(a)))
  }
}
