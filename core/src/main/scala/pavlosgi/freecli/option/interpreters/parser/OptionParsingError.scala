package pavlosgi.freecli.option.interpreters.parser

import pavlosgi.freecli.core._
import pavlosgi.freecli.core.parsing.Error
import pavlosgi.freecli.option.api.Field

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

case class OptionFieldMissing(field: Field)
  extends OptionParsingError  {

  val message = s"""Field ${field.shortDescription.yellow}, is missing"""
}

case class OptionFieldValueMissing(field: Field)
  extends OptionParsingError  {

  val message = s"Field value for ${field.shortDescription.yellow}, is missing"
}

case class FailedToDecodeOption(field: Field, error: StringDecoderError)
  extends OptionParsingError  {

  val message =
    s"Failed to decode option ${field.shortDescription}. ${error.message}"
}