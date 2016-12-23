package pavlosgi.freecli.command

import pavlosgi.freecli.core.formatting._

package object help {
  type Result[A] = CommandsHelp

  def commandHelp[A](dsl: CommandDsl[A]): String = {
    commandHelpWithPath(dsl)(Seq.empty)
  }

  def commandHelpWithPath[A](dsl: CommandDsl[A])(args: Seq[String]): String = {
    val commands = dsl.analyze(CommandHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |${commands.helpForPath(args.toList).result.display(2)}
       |""".stripMargin
  }
}
