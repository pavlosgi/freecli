package pavlosgi.freecli.option

import cats.data.NonEmptyList

import pavlosgi.freecli.option.api.Action
import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[Action, OptionParsingErrors, A]
  type OptionParsingErrors = NonEmptyList[OptionParsingError]
}
