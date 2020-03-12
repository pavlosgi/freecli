package freecli
package argument

import api.{Action, NoOp}
import freecli.parser.CliParser
import parser.ArgumentParsingErrors

object ops extends AllOps
trait AllOps extends dsl.Ops with parser.ParserOps with help.HelpOps {
  def runArgumentOrFail[A](dsl: ArgumentDsl[A])(args: Array[String]): A =
    CliParser.runOrFail[Action, ArgumentParsingErrors, A](
      args.toIndexedSeq,
      Some(argumentHelp(dsl)),
      { case n: NoOp.type => n.run() }
    )(parseArgument(dsl))
}
