package pavlosgi.freecli.option

import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[OptionParsingError, A]
}
