package pavlosgi.freecli.argument

import pavlosgi.freecli.argument.api.Action
import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[Action, ArgumentParsingError, A]
}
