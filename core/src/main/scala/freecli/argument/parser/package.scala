package freecli
package argument

import cats.data.NonEmptyList

import api.Action
import freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[Action, ArgumentParsingErrors, A]
  type ArgumentParsingErrors = NonEmptyList[ArgumentParsingError]
}
