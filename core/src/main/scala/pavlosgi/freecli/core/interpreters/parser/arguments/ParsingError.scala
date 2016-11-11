package pavlosgi.freecli.core.interpreters.parser.arguments

import cats.Show
import cats.syntax.show._

import pavlosgi.freecli.core.api.arguments.ArgumentDetails
import pavlosgi.freecli.core.api.arguments.StringDecoderError

sealed trait ParsingError {
  val description: String
}

object ParsingError {
  def fromStringDecoderError(error: StringDecoderError): StringDecoderParsingError =
    StringDecoderParsingError(error)

  implicit object showInstance extends Show[ParsingError] {
    override def show(f: ParsingError): String = f.description
  }
}

case class UnknownArgumentsParsingError(args: Seq[String])
  extends ParsingError  {

  val description = s"Unknown arguments passed ${args.mkString(", ")}"
}

case class ArgumentValueMissingParsingError(details: ArgumentDetails)
  extends ParsingError  {

  val description = s"Argument value for ${details.show} is missing"
}

case class StringDecoderParsingError(error: StringDecoderError)
  extends ParsingError  {

  val description =
    s"Field value for ${error.details.show} failed to decode from string. ${error.message}"
}