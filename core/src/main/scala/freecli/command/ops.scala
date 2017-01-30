package freecli
package command

import api.{Action, ConfigAction}
import parser.CommandParsingError
import freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends dsl.Ops with parser.ParserOps with help.HelpOps {
  def runCommandOrFail[A](dsl: CommandDsl[A])(args: Seq[String]): A = {
    CliParser.runOrFail[Action, CommandParsingError, A](
      args,
      Some(commandHelp(dsl)),
      { case c@ConfigAction(_, _, _) => c.run()
      })(
      parseCommand(dsl))
  }
}
