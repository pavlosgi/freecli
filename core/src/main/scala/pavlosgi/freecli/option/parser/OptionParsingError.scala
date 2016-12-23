package pavlosgi.freecli.option.parser

import pavlosgi.freecli.core.StringDecoderError
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

case class OptionFieldValueMissing(field: OptionField)
  extends OptionParsingError  {

  val message = s"Field value for ${field.shortDescription.yellow}, is missing"
}

case class FailedToDecodeOption(field: OptionField, error: StringDecoderError)
  extends OptionParsingError  {

  val message =
    s"Failed to decode option ${field.shortDescription}. ${error.message}"
}