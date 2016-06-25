package pavlosgi
package freecli
package commands
package help

import formatting._
import config.help.ConfigHelp

case class CommandHelpNode(name: String, config: ConfigHelp, help: CommandHelp)

case class CommandHelp(
  cmds: Seq[CommandHelpNode] = Seq.empty
){
  def asString(indentation: Int): String = {
    val cmdsAsString = cmds.map { c =>
      c.name.cyan.indent(indentation) + "\n" +
        c.config.asString(indentation + 1) +
        "\n" + c.help.asString(indentation + 1)

    }.mkString("\n\n")

    cmdsAsString.mkString("\n")
  }
}
