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
    dsl: ConfigDsl[T],
    optsLookAhead: Boolean = true):
    CliParser[Action, ConfigParsingErrors, T] = {

    dsl.foldMap(new ConfigParserInterpreter(optsLookAhead))
  }

  def parseConfig[T](
    dsl: ConfigDsl[T],
    optsLookAhead: Boolean = true):
    CliParser[Action, ConfigParsingErrors, T] = {

    parseConfigNonStrict(dsl, optsLookAhead).failIfNotAllArgumentsUsed(
      args => NonEmptyList.of(AdditionalArgumentsFound(args.map(_.name))))
  }
}
