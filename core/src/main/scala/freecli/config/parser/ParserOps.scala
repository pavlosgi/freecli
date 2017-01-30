package freecli
package config
package parser

import cats.data.NonEmptyList

import api.Action
import dsl.ConfigDsl
import freecli.parser.CliParser

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
