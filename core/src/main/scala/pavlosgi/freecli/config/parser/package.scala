package pavlosgi.freecli.config

import pavlosgi.freecli.config.api.Action
import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[Action, ConfigParsingError, A]
}
