package reverso.common

import cats.Eval
import cats.implicits._
import cats.kernel.Eq
import cats.laws.discipline.MonadTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class ListTSpec extends AnyFunSuiteLike with Configuration with FunSuiteDiscipline {
  implicit def eqListT[A: Eq]: Eq[ListT[Eval, A]] = Eq.by(_.value.value)

  implicit def arbListT[A: Arbitrary]: Arbitrary[ListT[Eval, A]] =
    Arbitrary(
      Gen.listOf(Arbitrary.arbitrary[A]).map(a => ListT[Eval, A](Eval.later(a)))
    )

  checkAll("ListT.MonadLaws", MonadTests[ListT[Eval, *]].monad[Int, Int, String])
}
