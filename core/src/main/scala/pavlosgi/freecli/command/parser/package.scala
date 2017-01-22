package pavlosgi.freecli.command

import pavlosgi.freecli.command.api.Action
import pavlosgi.freecli.parser.CliParser

package object parser extends Instances {
  type ParseResult[A] = CliParser[Action, CommandParsingError, A]
}
