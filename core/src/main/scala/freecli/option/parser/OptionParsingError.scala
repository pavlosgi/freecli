package freecli
package option
package parser

import core.api.StringDecoderError
import core.formatting._
import option.api.OptionField
import freecli.parser.DisplayErrors

sealed trait OptionParsingError {
  def message: String
}

object OptionParsingError {
  implicit object displayErrorsInstance extends DisplayErrors[OptionParsingErrors] {
    def display(errors: OptionParsingErrors): String = {
      errors.map(_.message).toList.mkString("\n")
    }
  }
}

case class AdditionalArgumentsFound(args: Seq[String])
  extends OptionParsingError  {

  def message = s"Additional arguments found: ${args.mkString(", ")}"
}

case class OptionFieldMissing(field: OptionField)
  extends OptionParsingError  {

  def message = s"""Field ${field.shortDescription.yellow}, is missing"""
}

case class ArgumentsDidNotMatchAnyOptions(args: Seq[String])
  extends OptionParsingError  {

  def message = s"""Arguments ${args.mkString(", ")} did not match any options"""
}

case class OptionFieldValueMissing(field: OptionField)
  extends OptionParsingError  {

  def message = s"Field value for ${field.shortDescription.yellow}, is missing"
}

case class OptionWasFollowedByMoreArguments(field: OptionField, args: Seq[String])
  extends OptionParsingError  {

  def message = s"Option ${field.shortDescription.yellow}, was followed by more arguments ${args.mkString(", ")}"
}

case class FailedToDecodeOption(field: OptionField, error: StringDecoderError)
  extends OptionParsingError  {

  def message =
    s"Failed to decode option ${field.shortDescription}. ${error.message}"
}