package pavlosgi.freecli.core.interpreters.help.config

import cats.syntax.show._

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.interpreters.help._

case class ConfigHelpState(options: OptionHelpState, arguments: ArgumentHelpState) {
  def display: String = {
    s"""
       |${"Usage".bold.underline}
       |
       |  Program [options] ${arguments.oneline}
       |
       |  ${"Options".bold}
       |
       |  ${options.display(2)}
       |
       |  ${"Arguments".bold}
       |
       |  ${arguments.display(2)}
       |
     """.stripMargin
  }

  def addArgument(a: ArgumentDetails): ConfigHelpState = {
    this.copy(arguments = ArgumentHelpState(this.arguments.arguments :+ a))
  }

  def addOption(o: OptionHelp): ConfigHelpState = {
    this.copy(options = OptionHelpState(this.options.options :+ o))
  }
}

object ConfigHelpState {
  def empty = ConfigHelpState(OptionHelpState.empty, ArgumentHelpState.empty)
}

case class ArgumentHelpState(arguments: Seq[ArgumentDetails]) {
  def display(indentation: Int): String = {
    arguments.map { a =>
      String.format("%-15s %s", a.show.yellow, a.description.fold("")(_.show))
    }.mkString("\n")
  }

  def oneline: String = arguments.map(a => s"<${a.show}>").mkString(" ")
}

object ArgumentHelpState {
  def empty = ArgumentHelpState(Seq.empty)
}

case class OptionHelpState(options: Seq[OptionHelp]) {
  def display(indentation: Int): String = {
    options.foldLeft(indent(indentation, "")) {
      (p, v) => p.newline + v.display(indentation)
    }
  }
}

object OptionHelpState {
  def empty = OptionHelpState(Seq.empty)
}

sealed trait OptionHelp {
  def display(indentation: Int): String
}

case class FieldHelp(field: Field) extends OptionHelp {
  override def display(indentation: Int): String = {
    val v = field match {
      case FieldNameOnly(name, description) =>
        String.format("%-20s %s", name.show.yellow, description.fold("")(_.show))

      case FieldAbbreviationOnly(abbr, description) =>
        String.format("%-20s %s", abbr.show.yellow, description.fold("")(_.show))

      case FieldNameAndAbbreviation(name, abbr, description) =>
        String.format(
          "%-15s, %-5% %s",
          name.show.yellow,
          abbr.show.yellow,
          description.fold("")(_.show))
    }

    indent(indentation, v)
  }
}

case class SubHelp(description: Description, config: ConfigHelpState)
  extends OptionHelp {

  override def display(indentation: Int): String = {
    s"""
       |${indent(indentation, description.show)}
       |
       |${config.options.display(indentation)}
     """.stripMargin
  }
}