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

    val cmdsAsSeq = cmds.map { c =>
      Seq(c.name.cyan.indent(indentation),
          c.config.asString(indentation + 1),
          c.help.asString(indentation + 1)).filter(_.nonEmpty)
    }

    cmdsAsSeq.map(_.mkString("\n")).mkString("\n\n")
  }
}

object CommandHelp {
  def legend: String = "Commands".cyan + " " + ConfigHelp.legend
}