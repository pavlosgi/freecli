package pavlosgi.freecli.option.interpreters.help

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core.Description
import pavlosgi.freecli.option.api._
import pavlosgi.freecli.core._

case class HelpState(options: List[OptionHelp])
object HelpState {
  implicit def monoidInstance = new Monoid[HelpState] {
    def combine(x: HelpState, y: HelpState): HelpState = {
      HelpState(x.options |+| y.options)
    }

    def empty = HelpState(List.empty)
  }

  def calculateMaxNameLength(o: HelpState): Int = {
    o.options.map {
      case OptionFieldHelp(f, _, _) =>
        OptionFieldHelp.displayName(f).lengthExclANSI

      case SubHelp(d, s) =>
        HelpState.calculateMaxNameLength(s)

    }.maximumOption.getOrElse(0)
  }

  def display(indentation: Int, value: HelpState, currMaxName: Option[Int] = None): String = {
    val maxNameLength =
      HelpState.calculateMaxNameLength(value).max(currMaxName.getOrElse(0))

    val res = value.options.zipWithIndex.map {
      case (f: OptionFieldHelp, _) =>
        OptionFieldHelp.display(indentation, maxNameLength, f)

      case (f: SubHelp, idx) if idx === 0 && value.options.size > 1 =>
        s"${SubHelp.display(indentation, f, Some(maxNameLength))}\n"

      case (f: SubHelp, idx) if idx === value.options.size - 1 =>
        s"\n${SubHelp.display(indentation, f, Some(maxNameLength))}"

      case (f: SubHelp, idx) if idx > 0 && idx < value.options.size - 1 =>
        s"\n${SubHelp.display(indentation, f, Some(maxNameLength))}\n"

    }.mkString("\n")

    if (res.charAt(res.length - 1) === '\n') {
      res.init
    } else res
  }
}


sealed trait OptionHelp

case class OptionFieldHelp(
  field: Field,
  required: Boolean,
  default: Option[String] = None)
  extends OptionHelp

object OptionFieldHelp {
  def displayName(field: Field): String = {
    field match {
      case FieldNameOnly(name, _) =>
        name.show.yellow

      case FieldAbbreviationOnly(abbr, _) =>
        abbr.show.yellow

      case FieldNameAndAbbreviation(name, abbr, _) =>
        s"${name.show.yellow}, ${abbr.show.yellow}"
    }
  }

  def display(indentation: Int, maxNameLength: Int, value: OptionFieldHelp): String = {
    val required = if (value.required) Some("Required".bold) else None
    val modifiers = value.default.map(v => s"Defaults to $v".bold).orElse(required)

    val res = value.field match {
      case f@FieldNameOnly(_, description) =>
        displayName(f) -> optionalPair(modifiers, description.map(_.show), " - ")

      case f@FieldAbbreviationOnly(_, description) =>
        displayName(f) -> optionalPair(modifiers, description.map(_.show), " - ")

      case f@FieldNameAndAbbreviation(_, _, description) =>
        displayName(f) ->
        optionalPair(modifiers, description.map(_.show), " - ")
    }

    val space = maxNameLength - res._1.lengthExclANSI + 2
    indent(indentation, res._1 + " " * space + res._2)
  }
}

case class SubHelp(description: Description, config: HelpState)
  extends OptionHelp

object SubHelp {
  def display(
    indentation: Int,
    value: SubHelp,
    currMaxNameLength: Option[Int] = None):
    String = {

    contentWithTitle(
      indent(indentation, value.description.show),
      HelpState.display(indentation, value.config, currMaxNameLength))
  }
}