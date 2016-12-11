package pavlosgi.freecli.command.interpreters.parser

import cats.data.NonEmptyList

import pavlosgi.freecli.command.api.CommandField
import pavlosgi.freecli.config.interpreters.parser.ConfigParsingError
import pavlosgi.freecli.core._
import pavlosgi.freecli.core.parsing.Error

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

case class FailedToParseConfig(configErrors: NonEmptyList[ConfigParsingError])
  extends CommandParsingError  {

  val message = s"Config errors, ${configErrors.map(_.message).toList.mkString(", ")}"
}

case class CommandNotFound(field: CommandField)
  extends CommandParsingError  {

  val message = s"${field.shortDescription.yellow} command not found"
}

case object NoCommandWasMatched extends CommandParsingError {
  override val message: String = "No command was matched"
}

case object MultipleCommandsMatched extends CommandParsingError {
  override val message: String = "Multiple commands matched"
}