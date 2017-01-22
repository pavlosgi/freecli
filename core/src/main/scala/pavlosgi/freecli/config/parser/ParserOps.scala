package pavlosgi.freecli.config.parser

import pavlosgi.freecli.config.api.Action
import pavlosgi.freecli.config.dsl.ConfigDsl
import pavlosgi.freecli.parser.CliParser

object ops extends ParserOps
trait ParserOps {
  private[freecli] def parseConfigNonStrict[T](
    dsl: ConfigDsl[T]):
    CliParser[Action, ConfigParsingError, T] = {

    dsl.foldMap(ConfigParserInterpreter)
  }

  def parseConfig[T](
    dsl: ConfigDsl[T]):
    CliParser[Action, ConfigParsingError, T] = {

    parseConfigNonStrict(dsl).failIfNotAllArgumentsUsed(
      args => AdditionalArgumentsFound(args.args.map(_.name)))
  }
}
