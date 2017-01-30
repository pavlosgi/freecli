package freecli
package argument
package help

import core.formatting._
import dsl.ArgumentDsl

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
