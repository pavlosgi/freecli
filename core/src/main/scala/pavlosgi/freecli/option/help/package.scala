package pavlosgi.freecli.option

import pavlosgi.freecli.core.formatting._

package object help {
  type HelpResult[A] = OptionsHelp

  def optionsHelp[A](dsl: OptionDsl[A]): String = {
    val result = dsl.analyze(OptionHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |  Program [options]
       |
       |${result.result.display(4)}
       |""".stripMargin
  }

}
