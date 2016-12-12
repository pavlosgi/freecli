package pavlosgi.freecli.command.interpreters

import cats.~>

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.config.interpreters.{help => C}

package object help {
  type Result[A] = CommandsHelp

  implicit def commandHelpInterpreter: Algebra ~> Result =
    new (Algebra ~> Result) {
      def apply[A](fa: Algebra[A]): Result[A] = {
        fa match {
          case PartialCmd(field, _, _) =>
            CommandsHelp.single(SimpleHelpCommand(field))

          case PartialCmdWithConfig(field, config, _, _) =>
            CommandsHelp.single(
              ConfigHelpCommand(field, config.analyze(C.configHelpInterpreter)))

          case PartialParentCmd(field, subs, _) =>
            CommandsHelp.single(
              SubHelpCommand(field, subs.analyze(commandHelpInterpreter)))

          case PartialParentCmdWithConfig(field, config, subs, _) =>
            CommandsHelp.single(ConfigSubHelpCommand(
              field,
              config.analyze(C.configHelpInterpreter),
              subs.analyze(commandHelpInterpreter)))
        }
      }
    }
}

