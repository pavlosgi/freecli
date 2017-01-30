package freecli
package command
package parser

import cats.{Alternative, Semigroup}
import cats.instances.all._
import cats.syntax.all._

import api.CommandField
import config.parser.ConfigParsingErrors
import core.formatting._
import freecli.parser.DisplayErrors

sealed trait CommandParsingError

object CommandParsingError {
  implicit object semigroupInstance extends Semigroup[CommandParsingError] {
    def combine(x: CommandParsingError, y: CommandParsingError): CommandParsingError = {
      (x, y) match {
        case (p: ParentCommandError, _) => p
        case (_, p: ParentCommandError) => p
        case (o1: OtherCommandErrors, o2: OtherCommandErrors) =>
          OtherCommandErrors.semigroupInstance.combine(o1, o2)
      }
    }
  }

  implicit object displayErrorsInstance extends DisplayErrors[CommandParsingError] {
    def display(errors: CommandParsingError): String = {

      def displayOtherErrors(
        parents: List[CommandField],
        errors: OtherCommandErrors): String = {

        parents match {
          case Nil => OtherCommandErrors.displayErrorsInstance.display(errors)
          case p =>
            s"""${p.map(_.shortDescription.yellow).mkString(" ")} errors
               |${indent(2, OtherCommandErrors.displayErrorsInstance.display(errors))}""".stripMargin
        }
      }

      def display(parents: List[CommandField], errors: CommandParsingError): String = {
        errors match {
          case p: ParentCommandError =>
            display(parents :+ p.parent, p.subErrors)

          case o: OtherCommandErrors =>
            displayOtherErrors(parents, o)
        }
      }

      display(List(), errors)
    }
  }
}

case class ParentCommandError(
  parent: CommandField,
  subErrors: CommandParsingError)
  extends CommandParsingError

case class OtherCommandErrors(
  additionalArgumentsFound: Option[AdditionalArgumentsFound] = None,
  failedToParseConfig: Option[FailedToParseConfig] = None ,
  commandNotFound: List[CommandNotFound] = List.empty,
  multipleCommandsMatched: Option[MultipleCommandsMatched.type] = None,
  noCommandWasMatched: Option[NoCommandWasMatched.type] = None)
  extends CommandParsingError

object OtherCommandErrors {
  implicit object semigroupInstance extends Semigroup[OtherCommandErrors] {
    def combine(x: OtherCommandErrors, y: OtherCommandErrors): OtherCommandErrors = {
      OtherCommandErrors(
        implicitly[Alternative[Option]].combineK(x.additionalArgumentsFound, y.additionalArgumentsFound),
        implicitly[Alternative[Option]].combineK(x.failedToParseConfig, y.failedToParseConfig),
        x.commandNotFound |+| y.commandNotFound,
        implicitly[Alternative[Option]].combineK(x.multipleCommandsMatched, y.multipleCommandsMatched),
        implicitly[Alternative[Option]].combineK(x.noCommandWasMatched, y.noCommandWasMatched))
    }
  }

  implicit object displayErrorsInstance extends DisplayErrors[OtherCommandErrors] {
    def display(errors: OtherCommandErrors): String = {
      errors match {
        case OtherCommandErrors(_, Some(configError), _, _, _) =>
          configError.message

        case OtherCommandErrors(_, None, notFound, _, _) =>
          s"Expected one of ${notFound.map(_.field.shortDescription.yellow).mkString(", ")}"

        case OtherCommandErrors(_, _, _, Some(err), _) =>
          err.message

        case OtherCommandErrors(_, _, _, _, Some(err)) =>
          err.message
      }
    }
  }
}

case class AdditionalArgumentsFound(args: Seq[String]) {
  def message: String =
    s"Additional arguments found: ${args.mkString(", ")}"
}

case class FailedToParseConfig(
  field: CommandField,
  configErrors: ConfigParsingErrors) {

  def message: String =
    s"""${field.shortDescription.yellow} command config errors
       |${indent(2, configErrors.map(_.message).toList.mkString(", "))}""".stripMargin

}

case class CommandNotFound(field: CommandField) {
  def message: String =
    s"${field.shortDescription.yellow} command not found"
}

case object NoCommandWasMatched {
  def message: String = "No command was matched"
}

case object MultipleCommandsMatched {
  def message: String = "Multiple commands matched"
}