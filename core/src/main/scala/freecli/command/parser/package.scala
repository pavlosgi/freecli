package freecli
package command

import api.Action
import freecli.parser.CliParser

package object parser extends Instances {
  type ParseResult[A] = CliParser[Action, CommandParsingError, A]
}
