package pavlosgi.freecli.core.dsl.command

import cats.Alternative
import cats.syntax.all._

import pavlosgi.freecli.core.api.command.Algebra
import pavlosgi.freecli.core.api.options.{Algebra => OptionsAlgebra}

abstract class CommandDsl[A] {
  def apply[F[_], C[_]](implicit ev: OptionsAlgebra[C], ev2: Algebra[F, C]): F[A]
}

object CommandDsl {
  implicit def dsl2FCommand[F[_], C[_], A](
    implicit ev: OptionsAlgebra[C],
    ev2: Algebra[F, C]):
    CommandDsl[A] => F[A] = {

    _.apply[F, C]
  }

  implicit def dsl2FCommand2[F[_], A](
    implicit ev: OptionsAlgebra[F],
    ev2: Algebra[F, F]):
    CommandDsl[A] => F[A] = {

    _.apply[F, F]
  }

  implicit def alternativeDsl: Alternative[CommandDsl] = {
    new Alternative[CommandDsl] {
      override def pure[A](x: A): CommandDsl[A] = new CommandDsl[A] {
        override def apply[F[_], C[_]](
          implicit ev: OptionsAlgebra[C],
          ev2: Algebra[F, C]):
          F[A] = x.pure[F]
      }

      override def ap[A, B]
        (ff: CommandDsl[(A) => B])
        (fa: CommandDsl[A]):
        CommandDsl[B] = new CommandDsl[B] {

        override def apply[F[_], C[_]](
          implicit ev: OptionsAlgebra[C],
          ev2: Algebra[F, C]):
          F[B] = {

          ff.apply[F, C].ap(fa.apply[F, C])
        }
      }

      override def empty[A]: CommandDsl[A] = new CommandDsl[A] {
        override def apply[F[_], C[_]](
          implicit ev: OptionsAlgebra[C],
          ev2: Algebra[F, C]):
          F[A] = {

          implicitly[Alternative[F]].empty
        }
      }

      override def combineK[A](x: CommandDsl[A], y: CommandDsl[A]): CommandDsl[A] = {
        new CommandDsl[A] {
          override def apply[F[_], C[_]](
            implicit ev2: OptionsAlgebra[C],
            ev: Algebra[F, C]):
            F[A] = {

            implicitly[Alternative[F]].combineK(x.apply[F, C], y.apply[F, C])
          }
        }
      }
    }
  }
  
}