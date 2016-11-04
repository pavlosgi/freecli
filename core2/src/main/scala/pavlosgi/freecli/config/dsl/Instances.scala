package pavlosgi
package freecli
package config
package dsl

import algebra._

import cats.syntax.all._
import cats.{Applicative, ~>}

trait Instances {
  implicit def applicativeDsl[G[_]: Plugin]: Applicative[ConfigDsl[G, ?]] =
    new Applicative[ConfigDsl[G, ?]] {
      override def pure[A](x: A): ConfigDsl[G, A] = new ConfigDsl[G, A] {
        def apply[F[_]: ConfigAlgebra[?[_], G]] = x.pure[F]
      }

      override def ap[A, B](ff: ConfigDsl[G, (A) => B])
                           (fa: ConfigDsl[G, A]): ConfigDsl[G, B] =
        new ConfigDsl[G, B] {
          override def apply[F[_] : ConfigAlgebra[?[_], G]]: F[B] =
            ff.apply[F].ap(fa.apply[F])
        }
    }

  implicit def fromConfigDsl[G[_]: Plugin, H[_]: Plugin, T]
    (c: ConfigDsl[G, T])
    (implicit ev1: G ~> H): ConfigDsl[H, T] = {

    new ConfigDsl[H, T] {
      override def apply[F[_] : ConfigAlgebra[?[_], H]]: F[T] = {
        val config = implicitly[ConfigAlgebra[F, H]]
        c.apply[F](new ConfigAlgebra[F, G] {
          override def arg[A, B](
            field: Field,
            f: (B) => A,
            default: Option[B]
          )(implicit ev: G[B]): F[A] =
            config.arg(field, f, default)(ev1.apply(ev))

          override def opt[A, B](
            field: Field,
            f: Option[B] => A
          )(implicit ev: G[B]): F[A] =
            config.opt(field, f)(ev1.apply(ev))

          override def sub[A](
                               field: SubField,
                               f: ApplyConfigAlgebra[G, A],
                               default: Option[A]
          ): F[A] = config.sub(field, f, default)

          override def pure[A](x: A): F[A] = config.pure(x)

          override def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B] =
            config.ap(ff)(fa)
        })
      }
    }
  }
}





