package freecli
package option

import cats.data.NonEmptyList

import option.api.Action
import freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[Action, OptionParsingErrors, A]
  type OptionParsingErrors = NonEmptyList[OptionParsingError]
}
