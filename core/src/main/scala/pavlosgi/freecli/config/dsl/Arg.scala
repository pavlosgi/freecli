package pavlosgi
package freecli
package config
package dsl

import cats.syntax.all._

import pavlosgi.freecli.config.algebra.{ConfigAlgebra, Field, Plugin}

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