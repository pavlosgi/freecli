package pavlosgi
package freecli
package commands
package dsl

import cats.syntax.all._
import cats.Alternative

import algebra._
import config.algebra.Plugin

trait Instances {
  implicit def alternativeDsl[G[_]: Plugin]: Alternative[CommandsDsl[G, ?]] =
    new Alternative[CommandsDsl[G, ?]] {
      override def combineK[A](x: CommandsDsl[G, A],
                               y: CommandsDsl[G, A]): CommandsDsl[G, A] = {

        new CommandsDsl[G, A] {
          override def apply[F[_] : CommandAlgebra[?[_], G]]: F[A] =
            x.apply[F].combineK(y.apply[F])
        }
      }

      override def pure[A](x: A): CommandsDsl[G, A] = new CommandsDsl[G, A] {
        def apply[F[_]: CommandAlgebra[?[_], G]] = x.pure[F]
      }

      override def ap[A, B](ff: CommandsDsl[G, (A) => B])
                           (fa: CommandsDsl[G, A]): CommandsDsl[G, B] = {

        new CommandsDsl[G, B] {
          override def apply[F[_] : CommandAlgebra[?[_], G]]: F[B] =
            ff.apply[F].ap(fa.apply[F])
        }
      }

      override def empty[A]: CommandsDsl[G, A] = new CommandsDsl[G, A] {
        override def apply[F[_]: CommandAlgebra[?[_], G]]: F[A] =
          implicitly[Alternative[F]].empty
      }
    }
}