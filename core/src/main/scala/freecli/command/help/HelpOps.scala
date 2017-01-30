package freecli
package command
package help

import dsl.CommandDsl
import core.formatting._

object ops extends HelpOps
trait HelpOps {
  def commandHelp[A](dsl: CommandDsl[A]): String = {
    val commands = dsl.analyze(CommandHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |${commands.result.display(2)}
       |""".stripMargin
  }
}
