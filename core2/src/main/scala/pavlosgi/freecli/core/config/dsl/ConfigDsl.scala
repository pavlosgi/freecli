package pavlosgi.freecli.core.dsl.config

import cats.Applicative
import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.config.Algebra

trait ConfigDsl[A] {
  def apply[F[_]: Algebra]: F[A]
}

object ConfigDsl {
  implicit def dsl2FA[A, F[_]: Algebra]: ConfigDsl[A] => F[A] = _.apply[F]

  implicit def applicativeDsl: Applicative[ConfigDsl] = {
    new Applicative[ConfigDsl] {
      override def pure[A](x: A): ConfigDsl[A] = new ConfigDsl[A] {
        override def apply[F[_] : Algebra]: F[A] = x.pure[F]
      }

      override def ap[A, B]
        (ff: ConfigDsl[(A) => B])
        (fa: ConfigDsl[A]):
        ConfigDsl[B] = new ConfigDsl[B] {

        override def apply[F[_]: Algebra]: F[B] = ff.apply[F].ap(fa.apply[F])
      }
    }
  }

  private[config] implicit class Merger[H <: HList](val c: ConfigDsl[H]) {
    def ::[B <: HList](dsl: Merger[B])(implicit ev: Prepend[B, H]): ConfigDsl[ev.Out] = {
      (dsl.c |@| c).map(_ ++ _)
    }

    def :->[B <: HList, Out <: HList](
      dsl: Merger[B])
     (implicit ev: Prepend.Aux[H, B, Out],
      ev2: shapeless.ops.hlist.Tupler[Out]):
      ConfigDsl[ev2.Out] = {

      (c |@| dsl.c).map((a, b) => ev2(a ++ b))
    }
  }

  object Merger {
    implicit def fromConfigDsl[T](c: ConfigDsl[T]): Merger[T :: HNil] =
      Merger(c.map(_ :: HNil))
  }

  private[config] class Builder[B] {
    def apply[L <: HList](dsl: ConfigDsl[L])(implicit gen: Generic.Aux[B, L]): ConfigDsl[B] = {
      implicitly[Applicative[ConfigDsl]].map(dsl)(gen.from)
    }
  }

}