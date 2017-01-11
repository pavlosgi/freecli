package pavlosgi.freecli.option

import pavlosgi.freecli.option.parser.OptionParserInterpreter
import pavlosgi.freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends dsl.Ops with parser.Ops with help.Ops {
  def parseOptionsOrHelp[A](args: Seq[String])(dsl: OptionDsl[A]): A = {
    CliParser.runOrFail(args, _ => optionsHelp(dsl))(dsl.foldMap(OptionParserInterpreter))
  }
}