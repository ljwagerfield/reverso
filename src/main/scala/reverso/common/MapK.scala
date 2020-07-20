package reverso.common

import cats.data.OptionT
import cats.implicits._
import cats.{~>, Functor}

object MapK {
  def optionTtoListT[F[_]: Functor]: OptionT[F, *] ~> ListT[F, *] =
    λ[OptionT[F, *] ~> ListT[F, *]](x => ListT(x.value.map(_.toList)))

  def fToListT[F[_]: Functor]: F ~> ListT[F, *] =
    λ[F ~> ListT[F, *]](x => ListT(x.map(_ :: Nil)))
}
