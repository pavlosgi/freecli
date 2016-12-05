package pavlosgi.freecli.core.interpreters.parser.command

import cats.syntax.show._

import pavlosgi.freecli.core.api.command.CommandField
import pavlosgi.freecli.core.interpreters.parser.config.ConfigParsingError

sealed trait CommandParsingError {
  val message: String
}

case class AdditionalArgumentsFound(args: Seq[String])
  extends CommandParsingError  {

  val message = s"Additional arguments found ${args.mkString(", ")}"
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