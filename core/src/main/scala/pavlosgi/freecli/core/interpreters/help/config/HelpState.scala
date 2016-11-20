package pavlosgi.freecli.core.interpreters.help.config

import cats.data.NonEmptyList

import pavlosgi.freecli.core.api.config.ArgumentDetails
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.interpreters.help._

case class HelpState(options: Option[OptionsHelp], arguments: Option[ArgumentsHelp]) {
  def addArgument(a: ArgumentDetails): HelpState = {
    arguments match {
      case None => this.copy(arguments = Some(ArgumentsHelp(NonEmptyList.of(a))))
      case Some(args) => this.copy(arguments = Some(ArgumentsHelp(args.arguments ++ List(a))))
    }
  }

  def addOption(o: OptionHelp): HelpState = {
    options match {
      case None => this.copy(options = Some(OptionsHelp(NonEmptyList.of(o))))
      case Some(args) => this.copy(options = Some(OptionsHelp(args.options ++ List(o))))
    }
  }
}

object HelpState {
  def empty = HelpState(None, None)

  def display(indentation: Int, s: HelpState): String = {
    val optsMax = s.options.fold(0)(OptionsHelp.calculateMaxNameLength)
    val argsMax = s.arguments.fold(0)(ArgumentsHelp.calculateMaxNameLength)
    val maxNameLength = optsMax.max(argsMax)

    Seq(
      optionalContentWithTitle(
        indent(indentation, "Options".bold),
        s.options.map(v => OptionsHelp.display(indentation + 2, maxNameLength, v))),

      optionalContentWithTitle(
        indent(indentation, "Arguments".bold),
        s.arguments.map(v => ArgumentsHelp.display(indentation + 2, maxNameLength, v)))

    ).flatten.mkString("\n\n")
  }
}

case class ArgumentsHelp(arguments: NonEmptyList[ArgumentDetails])

object ArgumentsHelp {
  def oneline(help: ArgumentsHelp): String = {
    help.arguments.toList.zipWithIndex.map {
      case (a, idx) => s"<${a.name.fold(s"arg${idx + 1}")(_.show)}>"
    }.mkString(" ")
  }

  def calculateMaxNameLength(a: ArgumentsHelp): Int = {
    a.arguments.toList.zipWithIndex.map {
      case (arg, idx) =>
        displayArgumentName(idx, arg).lengthExclANSI

    }.maximumOption.getOrElse(0)
  }

  def displayArgumentName(idx: Int, a: ArgumentDetails): String = {
    a.name.fold(s"arg${idx + 1}")(_.show).yellow
  }

  def display(indentation: Int, maxNameLength: Int, a: ArgumentsHelp) = {
    val res = a.arguments.toList.zipWithIndex.map {
      case (a, idx) =>
        val pair =
          a.name.fold(s"arg${idx + 1}")(_.show).yellow ->
          a.description.fold("")(_.show)

        val space = maxNameLength - pair._1.lengthExclANSI + 2
        pair._1 + " " * space + pair._2

    }.mkString("\n")

    indent(indentation, res)
  }
}

case class OptionsHelp(options: NonEmptyList[OptionHelp])
object OptionsHelp {
  def calculateMaxNameLength(o: OptionsHelp): Int = {
    o.options.toList.map {
      case OptionFieldHelp(f, _) =>
        OptionFieldHelp.displayName(f).lengthExclANSI

      case SubHelp(d, s) =>
        s.options.map(OptionsHelp.calculateMaxNameLength).getOrElse(0)

    }.maximumOption.getOrElse(0)
  }

  def display(indentation: Int, maxNameLength: Int, value: OptionsHelp): String = {
    val res = value.options.toList.foldLeft(List.empty[String]) {
      case (Nil, f@OptionFieldHelp(_, _)) =>
        List(OptionFieldHelp.display(indentation, maxNameLength, f))

      case (l, f@OptionFieldHelp(_, _)) =>
        l :+ OptionFieldHelp.display(indentation, maxNameLength, f)

      case (Nil, f@SubHelp(_, _)) =>
        SubHelp.display(indentation, f).map(v => s"$v\n").toList

      case (l, f@SubHelp(_, _)) =>
        l ++ SubHelp.display(indentation, f).map(v => s"\n$v\n").toList
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
  def display(indentation: Int, value: SubHelp): Option[String] = {
    optionalContentWithTitle(
      indent(indentation, value.description.show),
      value.config.options.map { v =>
        val d = OptionsHelp.calculateMaxNameLength(v)
        OptionsHelp.display(indentation, d, v)
      })
  }
}