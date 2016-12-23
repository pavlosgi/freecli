package pavlosgi.freecli.command.help

import cats.~>

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.config.{help => C}

object CommandHelpInterpreter extends (Algebra ~> Result) {
  def apply[A](fa: Algebra[A]): Result[A] = {
    fa match {
      case PartialCmd(field, _, _) =>
        CommandsHelp.single(SimpleHelpCommand(field))

      case PartialCmdWithConfig(field, config, _, _) =>
        CommandsHelp.single(
          ConfigHelpCommand(field, config.analyze(C.ConfigHelpInterpreter)))

      case PartialParentCmd(field, subs, _) =>
        CommandsHelp.single(
          SubHelpCommand(field, subs.analyze(CommandHelpInterpreter)))

      case PartialParentCmdWithConfig(field, config, subs, _) =>
        CommandsHelp.single(ConfigSubHelpCommand(
          field,
          config.analyze(C.ConfigHelpInterpreter),
          subs.analyze(CommandHelpInterpreter)))
    }
  }
}

