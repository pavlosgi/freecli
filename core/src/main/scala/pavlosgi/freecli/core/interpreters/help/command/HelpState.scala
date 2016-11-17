package pavlosgi.freecli.core.interpreters.help.command

import cats.syntax.all._

import pavlosgi.freecli.core.api.command.CommandField
import pavlosgi.freecli.core.interpreters.help.{config => C}
import pavlosgi.freecli.core.interpreters.help._

case class HelpState(commands: Seq[CommandHelp]) {
  def addCommandHelp(
    field: Option[CommandField] = None,
    config: Option[C.HelpState] = None,
    subs: Option[HelpState] = None):
    HelpState = {

    this.copy(commands = this.commands :+ CommandHelp(field, config, subs))
  }

  def display(indentation: Int): String = {
    commands.flatMap {
      case CommandHelp(Some(field), None, None) =>
        Some(indent(indentation, s"${field.name.show}"))

      case CommandHelp(Some(field), Some(conf), None) =>
        Some(s"${indent(indentation, s"${field.name.show} [options]")} ${conf.display(indentation + 2)}")

      case CommandHelp(Some(field), None, Some(subs)) =>
        Some(
          s"""${indent(indentation, field.name.show)}
             |
             |${indent(indentation + 2, "Commands".bold)}
             |${subs.display(indentation + 4)}""".stripMargin)

      case CommandHelp(Some(field), Some(conf), Some(subs)) =>
         Some(
           s"""${indent(indentation, s"${field.name.show} [options]")} ${conf.display(indentation + 2)}
              |
              |${indent(indentation + 2, "Commands".bold)}
              |${subs.display(indentation + 4)}""".stripMargin)

      case _ => None
    }.mkString("\n\n")
  }
}

object HelpState {
  def empty = HelpState(Seq.empty)
}

case class CommandHelp(
  field: Option[CommandField],
  config: Option[C.HelpState],
  subs: Option[HelpState])