package pavlosgi.freecli.command.help

import pavlosgi.freecli.command.dsl.CommandDsl
import pavlosgi.freecli.core.formatting._

trait Ops {
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
