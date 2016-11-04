package pavlosgi.freecli.core.interpreters.command.parser

import cats.Show
import cats.syntax.show._

import pavlosgi.freecli.core.api.command.CommandField
import pavlosgi.freecli.core.interpreters.config.parser.ParsingError

sealed trait CommandParsingError {
  val description: String
}

object CommandParsingError {
  implicit object showInstance extends Show[CommandParsingError] {
    override def show(f: CommandParsingError): String = f.description
  }
}

case class UnknownArgumentsCommandParsingError(args: Seq[String])
  extends CommandParsingError  {

  val description = s"Unknown arguments passed ${args.mkString(", ")}"
}

case class FailedToParseConfig(configError: ParsingError)
  extends CommandParsingError  {

  val description = s"Failed to parse config ${configError.show}"
}

case class CommandNotFound(field: CommandField)
  extends CommandParsingError  {

  val description = s"Command not found for ${field.show}"
}

case object NoCommandWasMatched extends CommandParsingError {
  override val description: String = "No command was matched"
}

case object MultipleCommandsMatched extends CommandParsingError {
  override val description: String = "Multiple commands matched"
}