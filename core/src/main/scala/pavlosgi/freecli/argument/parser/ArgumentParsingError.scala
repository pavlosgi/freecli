package pavlosgi.freecli.argument.parser

import pavlosgi.freecli.argument.api.ArgumentField
import pavlosgi.freecli.core.StringDecoderError
import pavlosgi.freecli.core.formatting._
import pavlosgi.freecli.parser.Error

sealed trait ArgumentParsingError {
  val message: String
}

object ArgumentParsingError {
  implicit object errorInstance extends Error[ArgumentParsingError] {
    def message(error: ArgumentParsingError): String = {
      error.message
    }
  }
}

case class AdditionalArgumentsFound(args: Seq[String])
  extends ArgumentParsingError  {

  val message = s"Additional arguments found: ${args.mkString(", ")}"
}

case class ArgumentValueMissing(field: ArgumentField)
  extends ArgumentParsingError  {

  val message = {
    field match {
      case ArgumentField(None, None) =>
        s"Argument is missing"

      case ArgumentField(Some(name), _) =>
        s"Argument ${name.value.yellow} is missing"

      case ArgumentField(None, Some(description)) =>
        s"""Argument with description "${description.value}" is missing"""
    }
  }
}

case class FailedToDecodeArgument(details: ArgumentField, error: StringDecoderError)
  extends ArgumentParsingError  {

  val message =
    s"Failed to decode argument ${details.shortDescription.yellow}. ${error.message}"
}