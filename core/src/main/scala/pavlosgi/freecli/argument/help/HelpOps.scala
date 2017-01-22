package pavlosgi.freecli.argument.help

import pavlosgi.freecli.argument.dsl.ArgumentDsl
import pavlosgi.freecli.core.formatting._

object ops extends HelpOps
trait HelpOps {
  def argumentHelp[A](dsl: ArgumentDsl[A]): String = {
    val arguments = dsl.analyze(ArgumentHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |  Program ${arguments.oneline.display()}
       |
       |${arguments.result.display(4)}
       |""".stripMargin
  }
}