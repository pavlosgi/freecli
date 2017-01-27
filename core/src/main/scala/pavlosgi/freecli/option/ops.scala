package pavlosgi.freecli.option

import pavlosgi.freecli.option.api.{Action, HelpAction, VersionAction}
import pavlosgi.freecli.option.parser.OptionParsingErrors
import pavlosgi.freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends dsl.Ops with parser.ParserOps with help.HelpOps {
  def runOptionOrFail[A](dsl: OptionDsl[A])(args: Seq[String]): A = {
    CliParser.runOrFail[Action, OptionParsingErrors, A](
      args,
      Some(optionHelp(dsl)),
      { case h: HelpAction.type => h.run(optionHelp(dsl))
        case v: VersionAction => v.run()
      })(
      parseOption(dsl))
  }
}