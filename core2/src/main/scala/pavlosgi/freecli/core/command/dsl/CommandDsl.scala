package pavlosgi.freecli.core.command.dsl

import cats.Alternative
import cats.syntax.all._

import pavlosgi.freecli.core.command.api.{Algebra, Command}

trait CommandDsl[A] {
  def apply[F[_]: Algebra]: F[A]
}

object CommandDsl {
  implicit def dsl2FCommandA[A, F[_]: Algebra]:
    CommandDsl[Command] => F[Command] = _.apply[F]

  implicit def alternativeDsl: Alternative[CommandDsl] = {
    new Alternative[CommandDsl] {
      override def pure[A](x: A): CommandDsl[A] = new CommandDsl[A] {
        override def apply[F[_] : Algebra]: F[A] = x.pure[F]
      }

      override def ap[A, B]
        (ff: CommandDsl[(A) => B])
        (fa: CommandDsl[A]):
        CommandDsl[B] = new CommandDsl[B] {

        override def apply[F[_]: Algebra]: F[B] = ff.apply[F].ap(fa.apply[F])
      }

      override def empty[A]: CommandDsl[A] = new CommandDsl[A] {
        override def apply[F[_] : Algebra]: F[A] =
          implicitly[Alternative[F]].empty
      }

      override def combineK[A](x: CommandDsl[A], y: CommandDsl[A]): CommandDsl[A] = {
        new CommandDsl[A] {
          override def apply[F[_] : Algebra]: F[A] =
            implicitly[Alternative[F]].combineK(x.apply[F], y.apply[F])
        }
      }
    }
  }

  private[command] implicit class Merger(val c: CommandDsl[Command]) {
    def |(dsl: Merger): CommandDsl[Command] = {
      implicitly[Alternative[CommandDsl]].combineK(dsl.c, c)
    }
  }
}