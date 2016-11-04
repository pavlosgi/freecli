package pavlosgi.freecli.core.interpreters.help

import cats.data._
import cats.syntax.show._
import cats.~>

import pavlosgi.freecli.core.api.command.{Algebra, Command, CommandField, PartialCommand}
import pavlosgi.freecli.core.interpreters.help.config.{Result => CResult, _}

package object command {

  type Result[A] = State[HelpState, Unit]

  def commandHelp[G](
    dsl: G)
   (implicit ev: G => Result[Command]): String = {

    "Usage".bold.underline.newlineLeft.newline.newline +
      ev(dsl).runS(HelpState(2, "")).value.text.newline
  }

  implicit object commandAlgebraHelp extends Algebra[Result[?], CResult[?]] {

    implicit def configNat: CResult[?] ~> Result[?] = {
      new (CResult[?] ~> Result[?]) {
        override def apply[A](fa: CResult[A]): Result[A] =
          fa
      }
    }

    def cmdHelp(c: CommandField) = c.show
    def cmdHelpWithConfig(c: CommandField, conf: HelpState) = {
      s"${c.show}\n\t${conf.text}"
    }

    override def partialCmd[P](
      field: CommandField,
      run: P => Unit):
      Result[PartialCommand[P]] = {

      for {
        hs <- State.get[HelpState]
        _  <- State.set(HelpState(hs.indentation, s"${hs.text}\n${cmdHelp(field)}"))
      } yield ()
    }

    override def partialCmdWithConfig[H[_], A, P](
      field: CommandField,
      config: H[A],
      run: A => P => Unit)
     (implicit ev: H[A] => CResult[A]):
      Result[PartialCommand[P]] = {

      for {
        hs <- State.get[HelpState]
        _  <- State.set(HelpState(hs.indentation + 2, s"${hs.text}\n${cmdHelp(field)}"))
        _  <- ev(config)
      } yield ()
    }

    override def partialParentCmd[P, G[_]](
      field: CommandField,
      subs: G[PartialCommand[P]])
     (implicit ev: G[PartialCommand[P]] => Result[PartialCommand[P]]):
      Result[PartialCommand[P]] = {

      for {
        hs   <- State.get[HelpState]
        _    <- State.set(HelpState(hs.indentation + 2, s"${hs.text}\n${cmdHelp(field)}"))
        _    <- ev(subs)
      } yield ()
    }

    override def partialParentCmdWithConfig[H[_], A, P, G[_]](
      field: CommandField,
      config: H[A],
      subs: G[A => PartialCommand[P]])
     (implicit ev: H[A] => CResult[A],
      ev2: (G[A => PartialCommand[P]]) => Result[A => PartialCommand[P]]):
      Result[PartialCommand[P]] = {

      for {
        hs   <- State.get[HelpState]
        _    <- State.set(HelpState(hs.indentation + 2, s"${hs.text}\n${cmdHelp(field)}"))
        _    <- ev(config)
        _    <- ev2(subs)
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

    override def empty[A]: Result[A] = State.set(HelpState(0, ""))

    override def combineK[A](
      x: Result[A],
      y: Result[A]):
      Result[A] = {

      for {
        _ <- x.get
        _ <- y.get
      } yield ()
    }
  }

}
