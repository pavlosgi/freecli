package pavlosgi.freecli.argument

import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[ArgumentParsingError, A]
}
