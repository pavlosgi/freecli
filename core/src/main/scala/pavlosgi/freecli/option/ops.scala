package pavlosgi.freecli.option

import pavlosgi.freecli.option.api.{Action, HelpAction, VersionAction}
import pavlosgi.freecli.option.parser.OptionParsingError
import pavlosgi.freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends dsl.Ops with parser.ParserOps with help.HelpOps {
  def parseOptionOrFail[A](args: Seq[String])(dsl: OptionDsl[A]): A = {
    CliParser.runOrFail[Action, OptionParsingError, A](
      args,
      optionHelp(dsl),
      { case h: HelpAction.type => h.run(optionHelp(dsl))
        case v: VersionAction => v.run()
      })(
      parseOption(dsl))
  }
}