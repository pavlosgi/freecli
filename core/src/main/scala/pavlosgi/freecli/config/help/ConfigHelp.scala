package pavlosgi
package freecli
package config
package help

import pavlosgi.freecli.formatting._

case class ConfigHelp(
  args: Seq[String] = Seq.empty,
  opts: Seq[String] = Seq.empty,
  subConfigs: Seq[(String, ConfigHelp)] = Seq.empty
){
  def asString(indentation: Int): String = {
    val argsAsString = args.map(_.bold.indent(indentation)).mkString("\n")
    val optsAsString = opts.map(_.indent(indentation)).mkString(s"\n")
    val subConfigsAsString = subConfigs.map { c =>
      c._1.cyan.indent(indentation) + "\n" + c._2.asString(indentation + 1)
    }.mkString("\n\n")

    val all = Seq(
      Seq(argsAsString, optsAsString, subConfigsAsString).mkString("\n"))

    all.filter(_.nonEmpty).mkString("\n")
  }
}
