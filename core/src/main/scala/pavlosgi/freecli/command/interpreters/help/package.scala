package pavlosgi.freecli.command.interpreters

import cats.kernel.Monoid
import cats.~>

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.command.dsl.CommandDsl
import pavlosgi.freecli.config.interpreters.{help => C}
import pavlosgi.freecli.core._

package object help {
  type Result[A] = HelpState

  def commandHelp[A](dsl: CommandDsl[A]): String = {

    val result = dsl.analyze(commandAlgebraHelp)

    s"""
     |${"Usage".bold.underline}
     |
     |${HelpState.display(2, result)}
     |
     |""".stripMargin
  }

  implicit def commandAlgebraHelp: Algebra ~> Result =
    new (Algebra ~> Result) {
      def apply[A](fa: Algebra[A]): Result[A] = {
        fa match {
          case PartialCmd(field, _, _) =>
            Monoid[HelpState].empty.addCommandHelp(Some(field))

          case PartialCmdWithConfig(field, config, _, _) =>
            Monoid[HelpState].empty.addCommandHelp(
              Some(field),
              Some(config.analyze(C.configAlgebraHelp)))

          case PartialParentCmd(field, subs, _) =>
            Monoid[HelpState].empty.addCommandHelp(
              Some(field),
              None,
              Some(subs.analyze(commandAlgebraHelp)))

          case PartialParentCmdWithConfig(field, config, subs, _) =>
            Monoid[HelpState].empty.addCommandHelp(
              Some(field),
              Some(config.analyze(C.configAlgebraHelp)),
              Some(subs.analyze(commandAlgebraHelp)))
        }
      }
    }
}

