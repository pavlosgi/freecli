package pavlosgi.freecli.arguments.interpreters.parser

import cats.Show
import cats.syntax.show._

import pavlosgi.freecli.arguments.api.ArgumentDetails
import pavlosgi.freecli.core.StringDecoderError

sealed trait ArgumentParsingError {
  val message: String
}

object ArgumentParsingError {
  implicit object showInstance extends Show[ArgumentParsingError] {
    override def show(f: ArgumentParsingError): String = f.message
  }
}

case class AdditionalArgumentsFound(args: Seq[String])
  extends ArgumentParsingError  {

  val message = s"Additional arguments found ${args.mkString(", ")}"
}

case class ArgumentValueMissing(details: ArgumentDetails)
  extends ArgumentParsingError  {

  val message = s"Argument ${details.show} missing"
}

case class FailedToDecodeArgument(details: ArgumentDetails, error: StringDecoderError)
  extends ArgumentParsingError  {

  val message = s"Failed to decode argument ${details.show}. ${error.message}"
}