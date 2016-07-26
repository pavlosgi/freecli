package pavlosgi
package freecli
package config
package dsl

import cats.syntax.all._

import pavlosgi.freecli.config.algebra.{ConfigAlgebra, Field, Plugin}

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