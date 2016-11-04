package pavlosgi
package freecli
package commands
package help

import algebra._
import dsl.CommandsDsl
import formatting._
import config.algebra.{ApplyConfigAlgebra, Plugin}
import config.help.{all => ConfigHelp}
import cats.{Show, ~>}
import cats.data.State

trait Operations {
  type CommandsPrinter[_] = State[CommandHelp, Unit]

  def usage[G[_]: Plugin]
    (p: CommandsDsl[G, Command])
    (implicit nat: G ~> Show): String = {

    s"${"Usage".underline} (${CommandHelp.legend})\n\n${genHelp(p).asString(1)}"
  }

  def genHelp[G[_]: Plugin]
    (p: CommandsDsl[G, Command])
    (implicit nat: G ~> Show): CommandHelp = {

    p.apply(helpAlgebra).runS(CommandHelp()).value
  }

  def helpAlgebra[G[_]: Plugin]
    (implicit nat: G ~> Show): CommandAlgebra[CommandsPrinter, G] = {

    new CommandAlgebra[CommandsPrinter, G] {
      override def pure[A](x: A): CommandsPrinter[A] = State.pure(())

      override def ap[A, B]
        (ff: CommandsPrinter[(A) => B])
        (fa: CommandsPrinter[A]): CommandsPrinter[B] = {

        for {
          h1 <- ff.get
          h2 <- fa.get
        } yield State.pure(CommandHelp(h1.cmds ++ h2.cmds))
      }

      override def empty[A]: CommandsPrinter[A] = State.set(CommandHelp())

      override def combineK[A]
        (x: CommandsPrinter[A], y: CommandsPrinter[A]): CommandsPrinter[A] = {

        for {
          xSt <- x.get
          ySt <- y.get
        } yield State.pure(CommandHelp(xSt.cmds ++ ySt.cmds))
      }

      override def cmd
        (field: CommandField,
         run: => Unit,
         f: ApplyCommandAlgebra[G, Command]): CommandsPrinter[Unit] = {

        for {
          res <- State.modify[CommandHelp](s => s.copy(cmds = s.cmds :+
                  CommandHelpNode(field.name.name, ConfigHelp.ConfigHelp(), genHelp(f))))

        } yield res
      }

      override def cmdWithConfig[A]
        (field: CommandField,
         config: ApplyConfigAlgebra[G, A],
         run: A => Unit,
         f: ApplyCommandAlgebra[G, Command]): CommandsPrinter[Unit] = {

        for {
          res <- State.modify[CommandHelp](s => s.copy(cmds = s.cmds :+
                  CommandHelpNode(field.name.name,
                                  ConfigHelp.genHelp(config),
                                  genHelp(f))))

        } yield res
      }
    }
  }
}

