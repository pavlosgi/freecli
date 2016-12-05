package pavlosgi.freecli.options.interpreters.help

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core.Description
import pavlosgi.freecli.options.api._
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
      case OptionFieldHelp(f, _) =>
        OptionFieldHelp.displayName(f).lengthExclANSI

      case SubHelp(d, s) =>
        HelpState.calculateMaxNameLength(s)

    }.maximumOption.getOrElse(0)
  }

  def display(indentation: Int, value: HelpState, currMaxName: Option[Int] = None): String = {
    val maxNameLength =
      HelpState.calculateMaxNameLength(value).max(currMaxName.getOrElse(0))

    val res = value.options.foldLeft(List.empty[String]) {
      case (Nil, f@OptionFieldHelp(_, _)) =>
        List(OptionFieldHelp.display(indentation, maxNameLength, f))

      case (l, f@OptionFieldHelp(_, _)) =>
        l :+ OptionFieldHelp.display(indentation, maxNameLength, f)

      case (Nil, f@SubHelp(_, _)) =>
        List(SubHelp.display(indentation, f, Some(maxNameLength)))

      case (l, f@SubHelp(_, _)) =>
        l :+ SubHelp.display(indentation, f, Some(maxNameLength))

    }.mkString("\n")

    if (res.charAt(res.length - 1) === '\n') {
      res.init
    } else res
  }
}


sealed trait OptionHelp

case class OptionFieldHelp(
  field: Field,
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
    val default = value.default.map(v => s"default = $v")

    val res = value.field match {
      case f@FieldNameOnly(_, description) =>
        displayName(f) -> optionalPair(default, description.map(_.show))

      case f@FieldAbbreviationOnly(_, description) =>
        displayName(f) -> optionalPair(default, description.map(_.show))

      case f@FieldNameAndAbbreviation(_, _, description) =>
        displayName(f) ->
        optionalPair(default, description.map(_.show))
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