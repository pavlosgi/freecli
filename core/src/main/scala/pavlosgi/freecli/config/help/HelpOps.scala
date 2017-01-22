package pavlosgi.freecli.config.help

import pavlosgi.freecli.config.dsl.ConfigDsl
import pavlosgi.freecli.core.formatting._

object ops extends HelpOps
trait HelpOps {
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
