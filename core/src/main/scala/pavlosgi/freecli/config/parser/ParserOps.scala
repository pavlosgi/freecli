package pavlosgi.freecli.config.parser

import cats.data.NonEmptyList

import pavlosgi.freecli.config.api.Action
import pavlosgi.freecli.config.dsl.ConfigDsl
import pavlosgi.freecli.parser.CliParser

object ops extends ParserOps
trait ParserOps {
  private[freecli] def parseConfigNonStrict[T](
    dsl: ConfigDsl[T]):
    CliParser[Action, ConfigParsingErrors, T] = {

    dsl.foldMap(ConfigParserInterpreter)
  }

  def parseConfig[T](
    dsl: ConfigDsl[T]):
    CliParser[Action, ConfigParsingErrors, T] = {

    parseConfigNonStrict(dsl).failIfNotAllArgumentsUsed(
      args => NonEmptyList.of(AdditionalArgumentsFound(args.map(_.name))))
  }
}
