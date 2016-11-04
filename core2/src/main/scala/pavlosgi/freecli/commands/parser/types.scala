package pavlosgi
package freecli
package commands
package parser

import config.{parser => ConfigParser}
import pavlosgi.freecli.commands.algebra.CommandField

import cats.Show
import cats.syntax.all._

trait Types {
  type ParsingError = parser.ParsingError
  type ConfigError = parser.ConfigError
  val ConfigError = parser.ConfigError
  val CommandNotMatched = parser.CommandNotMatched
  type CommandNotFound = parser.CommandNotFound
  val CommandNotFound = parser.CommandNotFound
  type InvalidArgs = parser.InvalidArgs
  val InvalidArgs = parser.InvalidArgs
}

sealed trait ParsingError

object ParsingError {
  implicit object showInstance extends Show[ParsingError] {
    override def show(f: ParsingError): String = {
      f match {
        case ConfigError(error) =>
          s"Command config error ${error.show}"

        case CommandNotMatched =>
          "Command not matched"

        case CommandNotFound(field) =>
          s"${field.name.show} command is missing"

        case InvalidArgs(args) =>
          s"Args ${args.mkString(",")} are invalid"
      }
    }
  }
}

case class ConfigError(error: ConfigParser.ParsingError) extends ParsingError
case object CommandNotMatched extends ParsingError
case class CommandNotFound(field: CommandField) extends ParsingError
case class InvalidArgs(args: Seq[String]) extends ParsingError
