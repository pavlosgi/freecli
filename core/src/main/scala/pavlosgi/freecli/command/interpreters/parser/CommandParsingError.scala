package pavlosgi.freecli.command.interpreters.parser

import cats.syntax.show._

import pavlosgi.freecli.command.api.CommandField
import pavlosgi.freecli.config.interpreters.parser.ConfigParsingError
import pavlosgi.freecli.core.Error

sealed trait CommandParsingError {
  val message: String
}

object CommandParsingError {
  implicit object errorInstance extends Error[CommandParsingError] {
    def message(error: CommandParsingError): String = {
      error.message
    }
  }
}

case class AdditionalArgumentsFound(args: Seq[String])
  extends CommandParsingError  {

  val message = s"Additional arguments found: ${args.mkString(", ")}"
}

case class FailedToParseConfig(configError: ConfigParsingError)
  extends CommandParsingError  {

  val message = s"Failed to parse config ${configError.message}"
}

case class CommandNotFound(field: CommandField)
  extends CommandParsingError  {

  val message = s"Command not found for ${field.show}"
}

case object NoCommandWasMatched extends CommandParsingError {
  override val message: String = "No command was matched"
}

case object MultipleCommandsMatched extends CommandParsingError {
  override val message: String = "Multiple commands matched"
}