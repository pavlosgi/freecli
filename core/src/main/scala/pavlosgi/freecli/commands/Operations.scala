package pavlosgi
package freecli
package commands

import dsl.CommandsDsl
import formatting._
import pavlosgi.freecli.config.algebra.Plugin
import pavlosgi.freecli.config.parser.Parser
import cats._
import cats.std.all._
import cats.syntax.all._
import cats.data.Validated

import pavlosgi.freecli.commands.algebra.Command

trait Operations
  extends help.Operations
  with parser.Operations
  with dsl.Operations {

  def parseOrExit[A, G[_]: Plugin]
                 (args: Seq[String])
                 (dsl: CommandsDsl[G, Command])
                 (implicit ev: G ~> Show, ev2: G ~> Parser): Command = {

    parse(args)(dsl) match {
      case Validated.Invalid(e) =>
        val errors =
          s"""
             |${usage(dsl)}
             |${"Errors".red.underline}
             |
             |${e.toList.map(er => er.show.indent(1)).mkString("\n")}"""
                .stripMargin

        println(errors)
        sys.exit(1)

      case Validated.Valid(r) => r
    }
  }

}
