package pavlosgi.freecli.argument.parser

import cats.data.NonEmptyList

import pavlosgi.freecli.argument.api.Action
import pavlosgi.freecli.argument.dsl.ArgumentDsl
import pavlosgi.freecli.parser.CliParser

object ops extends ParserOps
trait ParserOps {
  private[freecli] def parseArgumentNonStrict[T](
    dsl: ArgumentDsl[T]):
    CliParser[Action, ArgumentParsingErrors, T] = {

    dsl.foldMap(ArgumentParserInterpreter)
  }

  def parseArgument[T](
    dsl: ArgumentDsl[T]):
    CliParser[Action, ArgumentParsingErrors, T] = {

    parseArgumentNonStrict(dsl).failIfNotAllArgumentsUsed(
      args => NonEmptyList.of(AdditionalArgumentsFound(args.map(_.name))))
  }
}
