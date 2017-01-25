package pavlosgi.freecli.command.parser

import pavlosgi.freecli.command.api.Action
import pavlosgi.freecli.command.dsl.CommandDsl
import pavlosgi.freecli.parser.CliParser

object ops extends ParserOps
trait ParserOps {
  private[freecli] def parseCommandNonStrict[T](
    dsl: CommandDsl[T]):
    CliParser[Action, CommandParsingError, T] = {

    dsl.foldMap(CommandParserInterpreter)
  }

  def parseCommand[T](
    dsl: CommandDsl[T]):
    CliParser[Action, CommandParsingError, T] = {

    parseCommandNonStrict(dsl).failIfNotAllArgumentsUsed(args => OtherCommandErrors(
      additionalArgumentsFound = Some(AdditionalArgumentsFound(args.map(_.name)))))
  }
}
