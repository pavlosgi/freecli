package pavlosgi
package freecli
package config
package dsl

import algebra._

import cats.syntax.all._
import cats.{Applicative, ~>}

trait Types {
  type ConfigDsl[G[_], A] = ApplyConfigAlgebra[G, A]

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

    implicit def fromConfigDsl[G[_]: Plugin, H[_]: Plugin, A]
                              (c: ConfigDsl[G, A])
                              (implicit ev1: G ~> H): ConfigDsl[H, A] = {

      new ConfigDsl[H, A] {
        override def apply[F[_] : ConfigAlgebra[?[_], H]]: F[A] = {
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

case class Arg[B](field: Field, default: Option[B])
object Arg {
  implicit def a2dsl[G[_]: Plugin, B]
                    (a: Arg[B])
                    (implicit ev: G[B]): ConfigDsl[G, B] = {

    new ConfigDsl[G, B] {
      def apply[F[_]: ConfigAlgebra[?[_], G]] =
        implicitly[ConfigAlgebra[F, G]].arg(a.field, (b: B) => b, a.default)
    }
  }

  implicit class ArgOps[A](a: Arg[A]) {
    //TODO improve these
    def ap[G[_]: Plugin](implicit ev: G[A]) = Arg.a2dsl[G, A](a)
    def |@|[G[_]: Plugin, C](c: Arg[C])(implicit ev: G[A], ev2: G[C]) =
      Arg.a2dsl[G, A](a) |@| Arg.a2dsl[G, C](c)

    def |@|[G[_]: Plugin, C](c: Opt[C])(implicit ev: G[A], ev2: G[C]) =
      Arg.a2dsl[G, A](a) |@| Opt.o2dsl[G, C](c)

    def |@|[G[_]: Plugin, C](c: ConfigDsl[G, C])(implicit ev: G[A]) =
      Arg.a2dsl[G, A](a) |@| c
  }
}

case class Opt[B](field: Field)
object Opt {
  implicit def o2dsl[G[_]: Plugin, B]
                    (a: Opt[B])
                    (implicit ev: G[B]): ConfigDsl[G, Option[B]] = {

    new ConfigDsl[G, Option[B]] {
      def apply[F[_]: ConfigAlgebra[?[_], G]] =
        implicitly[ConfigAlgebra[F, G]].opt(
          a.field,
          (b: Option[B]) => b)
    }
  }

  implicit class OptOps[A](o: Opt[A]) {
    //TODO improve these
    def ap[G[_]: Plugin](implicit ev: G[A]) = Opt.o2dsl[G, A](o)
    def |@|[G[_]: Plugin, C](c: Arg[C])(implicit ev: G[A], ev2: G[C]) =
      Opt.o2dsl[G, A](o) |@| Arg.a2dsl[G, C](c)

    def |@|[G[_]: Plugin, C](c: Opt[C])(implicit ev: G[A], ev2: G[C]) =
      Opt.o2dsl[G, A](o) |@| Opt.o2dsl[G, C](c)

    def |@|[G[_]: Plugin, C](c: ConfigDsl[G, C])(implicit ev: G[A]) =
      Opt.o2dsl[G, A](o) |@| c
  }
}