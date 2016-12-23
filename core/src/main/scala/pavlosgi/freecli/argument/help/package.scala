package pavlosgi.freecli.argument

import pavlosgi.freecli.core.formatting._

package object help {
  type Result[A] = ArgumentsHelp

  def argumentsHelp[A](dsl: ArgumentDsl[A]): String = {
    val arguments = dsl.analyze(ArgumentHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |  Program ${arguments.oneline.display()}
       |
       |${arguments.result.display(4)}
       |""".stripMargin
  }
}
