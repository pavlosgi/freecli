package pavlosgi.freecli.command.interpreters

import cats.kernel.Monoid
import cats.~>

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.config.interpreters.{help => C}

package object help {
  type Result[A] = HelpState

  implicit def commandHelpInterpreter: Algebra ~> Result =
    new (Algebra ~> Result) {
      def apply[A](fa: Algebra[A]): Result[A] = {
        fa match {
          case PartialCmd(field, _, _) =>
            Monoid[HelpState].empty.addCommandHelp(Some(field))

          case PartialCmdWithConfig(field, config, _, _) =>
            Monoid[HelpState].empty.addCommandHelp(
              Some(field),
              Some(config.analyze(C.configHelpInterpreter)))

          case PartialParentCmd(field, subs, _) =>
            Monoid[HelpState].empty.addCommandHelp(
              Some(field),
              None,
              Some(subs.analyze(commandHelpInterpreter)))

          case PartialParentCmdWithConfig(field, config, subs, _) =>
            Monoid[HelpState].empty.addCommandHelp(
              Some(field),
              Some(config.analyze(C.configHelpInterpreter)),
              Some(subs.analyze(commandHelpInterpreter)))
        }
      }
    }
}

