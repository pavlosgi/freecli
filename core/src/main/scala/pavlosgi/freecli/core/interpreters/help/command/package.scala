package pavlosgi.freecli.core.interpreters.help

import cats.data._
import cats.~>

import pavlosgi.freecli.core.api.AlgebraDependency
import pavlosgi.freecli.core.api.command._
import pavlosgi.freecli.core.api.config.{Algebra => ConfigAlgebra}
import pavlosgi.freecli.core.interpreters.help.{config => C}

package object command {
  type Result[A] = State[HelpState, Unit]

  def commandHelp[G](
    dsl: G)
   (implicit ev: G => Result[Command]): String = {
      s"""
       |${"Usage".bold.underline}
       |
       |${ev(dsl).runS(HelpState.empty).value.display(2)}
       |
       |""".stripMargin
  }

  implicit def commandAlgebraHelp = {
    new Algebra[Result] {

      override def partialCmd[P](
        field: CommandField,
        run: P => Unit):
        Result[PartialCommand[P]] = {

        for {
          hs <- State.get[HelpState]
          _  <- State.set(hs.addCommandHelp(Some(field)))
        } yield ()
      }

      override def partialCmdWithConfig[H[_], C[_], A, P](
        field: CommandField,
        config: H[A],
        run: A => P => Unit)
       (implicit ev: AlgebraDependency[ConfigAlgebra, Result, C],
        ev2: H[A] => C[A]):
        Result[PartialCommand[P]] = {

        for {
          hs  <- State.get[HelpState]
          hsc <- ev.nat(ev2(config)).get
          conf = hsc.commands.headOption.flatMap(_.config)
          _   <- State.set(hs.addCommandHelp(Some(field), conf))
        } yield ()
      }

      override def partialParentCmd[P, G[_]](
        field: CommandField,
        subs: G[PartialCommand[P]])
       (implicit ev: G[PartialCommand[P]] => Result[PartialCommand[P]]):
        Result[PartialCommand[P]] = {

        for {
          hs  <- State.get[HelpState]
          shs <- ev(subs).get
          _   <- State.set(hs.addCommandHelp(Some(field), None, Some(shs)))
        } yield ()
      }

      override def partialParentCmdWithConfig[H[_], C[_], A, P, G[_]](
        field: CommandField,
        config: H[A],
        subs: G[A => PartialCommand[P]])
       (implicit ev: AlgebraDependency[ConfigAlgebra, Result, C],
        ev2: H[A] => C[A],
        ev3: G[A => PartialCommand[P]] => Result[A => PartialCommand[P]]):
        Result[PartialCommand[P]] = {

        for {
          hs  <- State.get[HelpState]
          hsc <- ev.nat(ev2(config)).get
          shs <- ev3(subs).get
          conf = hsc.commands.headOption.flatMap(_.config)
          _   <- State.set(hs.addCommandHelp(Some(field), conf, Some(shs)))
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
        State.set(HelpState(Seq.empty))

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

  implicit def configAlgebraDependency =
    new AlgebraDependency[ConfigAlgebra, Result, C.Result] {
      override def algebra: ConfigAlgebra[C.Result] = C.configAlgebraHelp
      override def nat: C.Result ~> Result = {
        new (C.Result ~> Result) {

          def apply[A](fa: C.Result[A]): Result[A] = {
            fa.transformS[HelpState](_ => C.HelpState.empty, (t, a) =>
              HelpState.empty.addCommandHelp(None, Some(a), None))
          }
        }
      }
    }
}

