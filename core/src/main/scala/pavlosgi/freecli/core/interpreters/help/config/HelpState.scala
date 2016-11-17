package pavlosgi.freecli.core.interpreters.help.config

import pavlosgi.freecli.core.api.config.ArgumentDetails

import cats.syntax.all._

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.interpreters.help._

case class HelpState(options: OptionsHelp, arguments: ArgumentsHelp) {


  def display(
    indentation: Int):
    String = {

    val optionsDisplay = options match {
      case OptionsHelp(Nil) => None
      case o@OptionsHelp(_) =>
        Some(
          s"""${indent(indentation, "Options".bold)}
             |${o.display(indentation + 2)}""".stripMargin)
    }

    val argumentsDisplay = arguments match {
      case ArgumentsHelp(Nil) => None
      case a@ArgumentsHelp(_) =>
        Some(
          s"""${indent(indentation, "Arguments".bold)}
             |${a.display(indentation + 2)}""".stripMargin)
    }

    Seq(Some(arguments.oneline), optionsDisplay, argumentsDisplay).flatten.mkString("\n\n")
  }

  def addArgument(a: ArgumentDetails): HelpState = {
    this.copy(arguments = ArgumentsHelp(this.arguments.arguments :+ a))
  }

  def addOption(o: OptionHelp): HelpState = {
    this.copy(options = OptionsHelp(this.options.options :+ o))
  }
}

object HelpState {
  def empty = HelpState(OptionsHelp.empty, ArgumentsHelp.empty)
}

case class ArgumentsHelp(arguments: Seq[ArgumentDetails]) {
  def display(indentation: Int): String = {
    arguments.zipWithIndex.map {
      case (a, idx) =>
      val res = String.format(
        "%-45s %s",
        a.placeholder.fold(s"arg${idx + 1}")(_.show).yellow.yellow,
        a.description.fold("")(_.show))

      indent(indentation, res)
    }.mkString("\n")
  }

  def oneline: String = {
    arguments.zipWithIndex.map {
      case (a, idx) => s"<${a.placeholder.fold(s"arg${idx + 1}")(_.show)}>"
    }.mkString(" ")
  }
}

object ArgumentsHelp {
  def empty = ArgumentsHelp(Seq.empty)
}

case class OptionsHelp(options: Seq[OptionHelp]) {
  def display(indentation: Int): String = {
    options.map(_.display(indentation)).mkString("\n")
  }
}

object OptionsHelp {
  def empty = OptionsHelp(Seq.empty)
}

sealed trait OptionHelp {
  def display(indentation: Int): String
}

case class OptionFieldHelp(
  field: Field,
  default: Option[String] = None)
  extends OptionHelp {

  def des(description: Option[Description], default: Option[String]): String = {
    default -> description match {
      case (Some(df), Some(d)) => s"default = $df, ${d.show}"
      case (Some(df), None) => s"default = $df"
      case (None, Some(d)) => s"${d.show}"
      case (None, None) => s""
    }
  }

  override def display(indentation: Int): String = {

    val v = field match {
      case FieldNameOnly(name, description) =>
        String.format("%-45s %s", name.show.yellow.yellow, des(description, default))

      case FieldAbbreviationOnly(abbr, description) =>
        String.format("%-45s %s", abbr.show.yellow.yellow, des(description, default))

      case FieldNameAndAbbreviation(name, abbr, description) =>
        String.format(
          "%-45s %s",
          s"${name.show.yellow}, ${abbr.show.yellow}",
          des(description, default))
    }

    indent(indentation, v)
  }
}

case class SubHelp(description: Description, config: HelpState)
  extends OptionHelp {

  override def display(indentation: Int): String = {
    s"""
       |${indent(indentation, description.show)}
       |${config.options.display(indentation)}
       |""".stripMargin
  }
}
