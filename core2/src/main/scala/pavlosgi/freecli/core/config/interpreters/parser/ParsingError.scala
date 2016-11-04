package pavlosgi.freecli.core.config.interpreters.parser

import cats.syntax.show._

import pavlosgi.freecli.core.api.config.{Field, StringDecoderError}

sealed trait ParsingError {
  val description: String
}

object ParsingError {
  def fromStringDecoderError(error: StringDecoderError): StringDecoderParsingError =
    StringDecoderParsingError(error)
}

case class UnknownArgumentsParsingError(args: Seq[String])
  extends ParsingError  {

  val description = s"Unknown arguments passed ${args.mkString(", ")}"
}

case class ConfigFieldValueMissingParsingError(field: Field)
  extends ParsingError  {

  val description = s"Field value for ${field.show} is missing"
}

case class StringDecoderParsingError(error: StringDecoderError)
  extends ParsingError  {

  val description =
    s"Field value for ${error.field.show} failed to decode from string. ${error.message}"
}