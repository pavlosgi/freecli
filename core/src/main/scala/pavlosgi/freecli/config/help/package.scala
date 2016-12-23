package pavlosgi.freecli.config

import pavlosgi.freecli.core.formatting._

package object help {
  type HelpResult[A] = ConfigHelp

  def configHelp[A](dsl: ConfigDsl[A]): String = {
    val config = dsl.analyze(ConfigHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |  Program [options] ${config.oneline.display(0)}
       |
       |${config.result.display(4)}
       |""".stripMargin
  }
}
