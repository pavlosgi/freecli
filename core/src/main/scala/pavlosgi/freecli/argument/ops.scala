package pavlosgi.freecli.argument

import pavlosgi.freecli.argument.api.{Action, NoOp}
import pavlosgi.freecli.argument.parser.ArgumentParsingErrors
import pavlosgi.freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends dsl.Ops with parser.ParserOps with help.HelpOps {
  def runArgumentOrFail[A](dsl: ArgumentDsl[A])(args: Seq[String]): A = {
    CliParser.runOrFail[Action, ArgumentParsingErrors, A](
      args,
      Some(argumentHelp(dsl)),
      { case n: NoOp.type => n.run() })(
      parseArgument(dsl))
  }
}