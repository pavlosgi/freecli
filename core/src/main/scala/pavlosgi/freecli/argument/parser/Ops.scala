package pavlosgi.freecli.argument.parser

import pavlosgi.freecli.argument.api.Action
import pavlosgi.freecli.argument.dsl.ArgumentDsl
import pavlosgi.freecli.parser.CliParser

object ops extends ParserOps
trait ParserOps {
  private[freecli] def parseArgumentNonStrict[T](
    dsl: ArgumentDsl[T]):
    CliParser[Action, ArgumentParsingError, T] = {

    dsl.foldMap(ArgumentParserInterpreter)
  }

  def parseArgument[T](
    dsl: ArgumentDsl[T]):
    CliParser[Action, ArgumentParsingError, T] = {

    parseArgumentNonStrict(dsl).failIfNotAllArgumentsUsed(
      args => AdditionalArgumentsFound(args.args.map(_.name)))
  }
}
