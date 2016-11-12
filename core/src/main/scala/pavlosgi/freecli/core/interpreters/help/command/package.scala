package pavlosgi.freecli.core.interpreters.help

import cats.data._
import cats.syntax.show._
import cats.~>

import pavlosgi.freecli.core.api.AlgebraDependency
import pavlosgi.freecli.core.api.command._
import pavlosgi.freecli.core.api.config.{Algebra => ConfigAlgebra}
import pavlosgi.freecli.core.interpreters.help.config.{Result => CResult}

package object command {
  type Result[A] = State[HelpState, Unit]

  implicit def configAlgebraDependency =
    new AlgebraDependency[ConfigAlgebra, Result, CResult] {
      override def algebra: ConfigAlgebra[CResult] = ???
      override def nat: ~>[CResult, Result] = ???
    }

  def commandHelp[G](
    dsl: G)
   (implicit ev: G => Result[Command]): String = {
    (for {
      _ <- Result.newline
      _ <- Result.append("Usage".bold.underline)
      _ <- Result.newline
      _ <- Result.newline
      _ <- ev(dsl)
      _ <- Result.newline
    } yield ()).runS(HelpState(2, "")).value.text
  }

  implicit def commandAlgebraHelp = {
    new Algebra[Result] {
      def cmdHelp(c: CommandField) = c.show
      def cmdHelpWithConfig(c: CommandField, conf: HelpState) = {
        s"${c.show}\n\t${conf.text}"
      }

      override def partialCmd[P](
        field: CommandField,
        run: P => Unit):
        Result[PartialCommand[P]] = {

        getCommandFieldHelp(field)
      }

      override def partialCmdWithConfig[H[_], C[_], A, P](
        field: CommandField,
        config: H[A],
        run: A => P => Unit)
       (implicit ev: AlgebraDependency[ConfigAlgebra, Result, C],
        ev2: H[A] => C[A]):
        Result[PartialCommand[P]] = {

        for {
          _  <- getCommandFieldHelp(field)
          _  <- Result.indentation(_ + 2)
          _  <- ev.nat(ev2(config))
          _  <- Result.indentation(_ - 2)
        } yield ()
      }

      override def partialParentCmd[P, G[_]](
        field: CommandField,
        subs: G[PartialCommand[P]])
       (implicit ev: G[PartialCommand[P]] => Result[PartialCommand[P]]):
        Result[PartialCommand[P]] = {

        for {
          _    <- getCommandFieldHelp(field)
          _    <- Result.indentation(_ + 2)
          _    <- ev(subs)
          _    <- Result.indentation(_ - 2)
        } yield ()
      }

      override def partialParentCmdWithConfig[H[_], C[_], A, P, G[_]](
        field: CommandField,
        config: H[A],
        subs: G[A => PartialCommand[P]])
       (implicit ev: AlgebraDependency[ConfigAlgebra, Result, C],
        ev2: H[A] => C[A],
        ev4: G[A => PartialCommand[P]] => Result[A => PartialCommand[P]]):
        Result[PartialCommand[P]] = {

        for {
          _    <- getCommandFieldHelp(field)
          _    <- Result.indentation(_ + 2)
          _    <- ev.nat(ev2(config))
          _    <- Result.indentation(_ - 2)
          _    <- Result.newline
          _    <- Result.indentation(_ + 2)
          _    <- ev4(subs)
          _    <- Result.indentation(_ - 2)
        } yield ()
      }

      override def pure[A](x: A): Result[A] = State.pure(())

      override def ap[A, B](
        ff: Result[A => B])
       (fa: Result[A]):
        Result[B] = {

        for {
          ff1 <- ff.get
          fa1 <- fa.get
        } yield ()
      }

      override def empty[A]: Result[A] =
        State.set(HelpState(0, ""))

      override def combineK[A](
        x: Result[A],
        y: Result[A]):
        Result[A] = {

        for {
          _ <- x.get
          _ <- Result.newline
          _ <- y.get
        } yield ()
      }

      def getCommandFieldHelp(field: CommandField): Result[Unit] = {
        Result.appendAtIndentationLn(field match {
          case CommandField(name, description) =>
            String.format("%-15s   %s", name.show.bold, description.fold("")(_.show))
        })
      }
    }
  }
}

