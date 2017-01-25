package pavlosgi.freecli.config

import cats.data.NonEmptyList

import pavlosgi.freecli.config.api.Action
import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[Action, NonEmptyList[ConfigParsingError], A]
  type ConfigParsingErrors = NonEmptyList[ConfigParsingError]
}
