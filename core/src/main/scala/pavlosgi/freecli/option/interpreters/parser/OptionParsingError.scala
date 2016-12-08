package pavlosgi.freecli.option.interpreters.parser

import cats.Show
import cats.syntax.show._

import pavlosgi.freecli.core.StringDecoderError
import pavlosgi.freecli.option.api.Field

sealed trait OptionParsingError {
  val message: String
}

object OptionParsingError {
  implicit object showInstance extends Show[OptionParsingError] {
    override def show(f: OptionParsingError): String = f.message
  }
}

case class AdditionalOptionsFound(args: Seq[String])
  extends OptionParsingError  {

  val message = s"Additional options found ${args.mkString(", ")}"
}

case class OptionFieldMissing(field: Field)
  extends OptionParsingError  {

  val message = s"Field ${field.show} is missing"
}

case class OptionFieldValueMissing(field: Field)
  extends OptionParsingError  {

  val message = s"Field value for ${field.show} is missing"
}

case class FailedToDecodeOption(field: Field, error: StringDecoderError)
  extends OptionParsingError  {

  val message =
    s"Failed to decode option ${field.show}. ${error.message}"
}