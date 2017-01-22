package pavlosgi.freecli.option

import pavlosgi.freecli.option.api.Action
import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[Action, OptionParsingError, A]
}
