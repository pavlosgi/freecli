package pavlosgi.freecli.command

import pavlosgi.freecli.command.api.{Action, ConfigAction}
import pavlosgi.freecli.command.parser.CommandParsingError
import pavlosgi.freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends dsl.Ops with parser.ParserOps with help.HelpOps {
  def parseCommandOrFail[A](args: Seq[String])(dsl: CommandDsl[A]): A = {
    CliParser.runOrFail[Action, CommandParsingError, A](
      args,
      commandHelp(dsl),
      { case c@ConfigAction(_, _, _) => c.run()
      })(
      parseCommandNonStrict(dsl))
  }
}
