package pavlosgi.freecli.argument.parser

import cats.data.NonEmptyList

import pavlosgi.freecli.argument.api.ArgumentField
import pavlosgi.freecli.core.api.StringDecoderError
import pavlosgi.freecli.core.formatting._
import pavlosgi.freecli.parser.DisplayErrors

sealed trait ArgumentParsingError {
  def message: String
}

object ArgumentParsingError {
  implicit object displayErrorsInstance extends DisplayErrors[NonEmptyList[ArgumentParsingError]] {
    def display(errors: NonEmptyList[ArgumentParsingError]): String = {
      errors.map(_.message).toList.mkString("\n")
    }
  }
}

case class AdditionalArgumentsFound(args: Seq[String])
  extends ArgumentParsingError  {

  def message = s"Additional arguments found: ${args.mkString(", ")}"
}

case class ArgumentValueMissing(field: ArgumentField)
  extends ArgumentParsingError  {

  def message = {
    field match {
      case ArgumentField(None, None) =>
        s"Argument is missing"

      case ArgumentField(Some(name), _) =>
        s"Argument ${name.value.yellow} is missing"

      case ArgumentField(None, Some(description)) =>
        s"""Argument ${description.value.yellow} is missing"""
    }
  }
}

case class FailedToDecodeArgument(details: ArgumentField, error: StringDecoderError)
  extends ArgumentParsingError  {

  def message =
    s"Failed to decode argument ${details.shortDescription.yellow}. ${error.message}"
}