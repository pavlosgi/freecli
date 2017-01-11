package pavlosgi.freecli.command

import pavlosgi.freecli.parser.CliParser

package object parser extends Instances {
  type ParseResult[A] = CliParser[CommandParsingError, A]
}
