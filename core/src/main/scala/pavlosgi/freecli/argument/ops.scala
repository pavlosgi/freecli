package pavlosgi.freecli.argument

import pavlosgi.freecli.argument.parser.ArgumentParserInterpreter
import pavlosgi.freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends dsl.Ops with parser.Ops with help.Ops {
  def parseArgumentsOrHelp[A](args: Seq[String])(dsl: ArgumentDsl[A]): A = {
    CliParser.runOrFail(args, _ => argumentsHelp(dsl))(dsl.foldMap(ArgumentParserInterpreter))
  }
}