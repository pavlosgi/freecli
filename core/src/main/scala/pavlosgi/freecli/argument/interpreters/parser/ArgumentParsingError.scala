package pavlosgi.freecli.argument.interpreters.parser

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

case class ArgumentValueMissing(details: ArgumentField)
  extends ArgumentParsingError  {

  val message = s"Argument ${details.shortDescription.yellow}, missing"
}

case class FailedToDecodeArgument(details: ArgumentField, error: StringDecoderError)
  extends ArgumentParsingError  {

  val message =
    s"Failed to decode argument ${details.shortDescription.yellow}. ${error.message}"
}