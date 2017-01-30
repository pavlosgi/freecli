package freecli
package argument
package parser

import cats.data.NonEmptyList

import api.Action
import dsl.ArgumentDsl
import freecli.parser.CliParser

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
