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

    this.copy(commands = this.commands ++ List(CommandHelp(field, config, subs)))
  }
}

object HelpState {
  def empty = HelpState(Seq.empty)

  def display(indentation: Int, h: HelpState): String = {
    h.commands.map {
      case CommandHelp(Some(field), None, None) =>
        indent(indentation, field.name.show.yellow)

      case CommandHelp(Some(field), Some(conf), None) =>
        displayFieldWithConfig(indentation, field, conf)

      case CommandHelp(Some(field), None, Some(subs)) =>
        s"""${indent(indentation, field.name.show.yellow)}
           |
           |${indent(indentation + 2, contentWithTitle("Commands".bold, HelpState.display(2, subs)))}""".stripMargin

      case CommandHelp(Some(field), Some(conf), Some(subs)) =>
        s"""${displayFieldWithConfig(indentation, field, conf)}
           |
           |${indent(indentation + 2, contentWithTitle("Commands".bold, HelpState.display(2, subs)))}""".stripMargin

      case _ => ""

    }.filter(_.nonEmpty).mkString("\n\n")
  }

  def displayFieldWithConfig(indentation: Int, f: CommandField, s: C.HelpState): String = {
    val argsOneLine = s.arguments.map(C.ArgumentsHelp.oneline)

    Seq(
      Some(s"${indent(indentation, f.name.show.yellow)} $argsOneLine"),
      optionalContentWithTitle(
        indent(
          indentation + 2, "Description".bold),
          f.description.map(_.show).map(v => indent(indentation + 4, v))),

      Some(C.HelpState.display(indentation + 2, s))

    ).flatten.mkString("\n\n")
  }
}

case class CommandHelp(
  field: Option[CommandField],
  config: Option[C.HelpState],
  subs: Option[HelpState])