package pavlosgi.freecli.argument.interpreters.parser

import cats.syntax.show._

import pavlosgi.freecli.argument.api.ArgumentDetails
import pavlosgi.freecli.core.{Error, StringDecoderError}

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

case class ArgumentValueMissing(details: ArgumentDetails)
  extends ArgumentParsingError  {

  val message = s"Argument ${details.show}, missing"
}

case class FailedToDecodeArgument(details: ArgumentDetails, error: StringDecoderError)
  extends ArgumentParsingError  {

  val message = s"Failed to decode argument ${details.show}. ${error.message}"
}