package pavlosgi.freecli.argument

import cats.data.NonEmptyList

import pavlosgi.freecli.argument.api.Action
import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[Action, ArgumentParsingErrors, A]
  type ArgumentParsingErrors = NonEmptyList[ArgumentParsingError]
}
