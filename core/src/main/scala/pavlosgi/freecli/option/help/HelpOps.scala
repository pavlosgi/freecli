package pavlosgi.freecli.option.help

import pavlosgi.freecli.core.formatting._
import pavlosgi.freecli.option.dsl.OptionDsl

object ops extends HelpOps
trait HelpOps {
  def optionHelp[A](dsl: OptionDsl[A]): String = {
    val result = dsl.analyze(OptionHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |  Program [options]
       |
       |${result.result.display(4)}
       |""".stripMargin
  }
}
