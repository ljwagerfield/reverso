package reverso.common

import cats.{Applicative, Monad, Traverse}
import cats.implicits._

/**
  * The controversial ListT type!
  *
  * Warning: use an M[_] that provides stack-safety, such as 'Eval', 'IO' or similar. (Do not use 'Id', 'Option', etc.)
  *
  * Re: https://typelevel.org/cats/faq.html#listt
  *
  *   ListT was never officially supported for the following reasons: 1) it suffers from associativity issues, 2) it
  *   suffers from stack safety, 3) even if the prior issues were addressed, it would suffer from poor performance.
  *
  * Addressing these issues: this implementation _does_ satisfy the monad laws (there is a cats-law test checked in),
  * and providing a stack-safe M[_] is used, it is stack-safe. The performance issues quoted probably relate more
  * fundamentally to the exponential nature of flat-mapping a list N times. If so, and if you cannot avoid this general
  * concept in your code (e.g. you're building a tree, which is exponential by nature), then using ListT is probably no
  * worse for performance than any alternative you might arrive at (bar micro-optimised alternatives).
  *
  * Obeys the laws defined in cats.laws.MonadLaws: see 'ListTSpec'
  */
case class ListT[M[_], A](value: M[List[A]])

object ListT {

  def fromList[M[_]: Applicative, A](list: List[A]): ListT[M, A] =
    ListT(list.pure[M])

  implicit def listTMonad[M[_]: Monad]: Monad[ListT[M, *]] =
    new Monad[ListT[M, *]] {

      override def flatMap[A, B](fa: ListT[M, A])(f: A => ListT[M, B]): ListT[M, B] =
        ListT(
          Monad[M].flatMap[List[A], List[B]](fa.value)(list =>
            Traverse[List].flatTraverse[M, A, B](list)(a => f(a).value)
          )
        )

      override def pure[A](a: A): ListT[M, A] = ListT(Monad[M].pure(List(a)))

      override def tailRecM[A, B](a: A)(f: A => ListT[M, Either[A, B]]): ListT[M, B] =
        flatMap(f(a)) { // Safe if M[_] grants stack-safe nested flatmaps (https://typelevel.org/cats/faq.html#tailrecm)
          case Right(b)    => pure(b)
          case Left(nextA) => tailRecM(nextA)(f)
        }

    }

}
