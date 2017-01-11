package pavlosgi.freecli.command

import pavlosgi.freecli.command.parser.CommandParserInterpreter
import pavlosgi.freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends dsl.Ops with parser.Ops with help.Ops {
  def parseCommandOrHelp[A](args: Seq[String])(dsl: CommandDsl[A]): A = {
    CliParser.runOrFail(
      args,
      commandHelpWithPath(dsl))(dsl.foldMap(CommandParserInterpreter))
  }
}
