package pavlosgi.freecli.option.parser

import pavlosgi.freecli.core.api.StringDecoderError
import pavlosgi.freecli.core.formatting._
import pavlosgi.freecli.option.api.OptionField
import pavlosgi.freecli.parser.Error

sealed trait OptionParsingError {
  val message: String
}

object OptionParsingError {
  implicit object errorInstance extends Error[OptionParsingError] {
    def message(error: OptionParsingError): String = {
      error.message
    }
  }
}

case class AdditionalArgumentsFound(args: Seq[String])
  extends OptionParsingError  {

  val message = s"Additional arguments found: ${args.mkString(", ")}"
}

case class OptionFieldMissing(field: OptionField)
  extends OptionParsingError  {

  val message = s"""Field ${field.shortDescription.yellow}, is missing"""
}

case class ArgumentsDidNotMatchAnyOptions(args: Seq[String])
  extends OptionParsingError  {

  val message = s"""Arguments ${args.mkString(", ")} did not match any options"""
}

case class OptionFieldValueMissing(field: OptionField)
  extends OptionParsingError  {

  val message = s"Field value for ${field.shortDescription.yellow}, is missing"
}

case class OptionWasFollowedByMoreArguments(field: OptionField, args: Seq[String])
  extends OptionParsingError  {

  val message = s"Option ${field.shortDescription.yellow}, was followed by more arguments ${args.mkString(", ")}"
}

case class FailedToDecodeOption(field: OptionField, error: StringDecoderError)
  extends OptionParsingError  {

  val message =
    s"Failed to decode option ${field.shortDescription}. ${error.message}"
}