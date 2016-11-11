package pavlosgi.freecli.core.dsl.options

import cats.Applicative
import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.{LeftFolder, Prepend}

import pavlosgi.freecli.core.api.options.Algebra
import pavlosgi.freecli.core.dsl.generic

trait OptionsDsl[A] {
  def apply[F[_]: Algebra]: F[A]
}

object OptionsDsl {
  implicit def dsl2FA[A, F[_]: Algebra]: OptionsDsl[A] => F[A] = _.apply[F]

  implicit def applicativeDsl: Applicative[OptionsDsl] = {
    new Applicative[OptionsDsl] {
      override def pure[A](x: A): OptionsDsl[A] = new OptionsDsl[A] {
        override def apply[F[_] : Algebra]: F[A] = x.pure[F]
      }

      override def ap[A, B]
        (ff: OptionsDsl[(A) => B])
        (fa: OptionsDsl[A]):
        OptionsDsl[B] = new OptionsDsl[B] {

        override def apply[F[_]: Algebra]: F[B] = ff.apply[F].ap(fa.apply[F])
      }
    }
  }

  object merge extends Poly2 {
    implicit def fromHList[L <: HList, H <: HList]
     (implicit ev: Prepend[L, H]) = {

      at[L, H]((l, h) => l ++ h)
    }

    implicit def fromNonHList[L <: HList, H]
     (implicit ev: Prepend[L, H :: HNil],
      ev2: H <:!< HList) = {

      at[L, H]((l, h) => l :+ h)
    }
  }

  implicit class Merger[H](private val c: OptionsDsl[H]) {
    def ::[L, Out <: HList](
      dsl: Merger[L])
     (implicit ev: LeftFolder.Aux[L :: H :: HNil, HNil, merge.type, Out]):
      OptionsDsl[Out] = {

      (dsl.c |@| c).map((l, h) => ev.apply(l :: h :: HNil, HNil))
    }
  }

  class Apply[T] {
    def apply[Conf](
      f: OptionsDsl[Conf])
     (implicit folder: LeftFolder.Aux[Conf :: HNil, Option[T], generic.type, T]):
      OptionsDsl[T] = {

      f.map(c => folder(c :: HNil, Option.empty[T]))
    }
  }

}