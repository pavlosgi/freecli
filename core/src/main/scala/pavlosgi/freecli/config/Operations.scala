package pavlosgi
package freecli
package config

import cats._
import cats.data.Validated
import cats.syntax.all._
import cats.std.all._

import pavlosgi.freecli.config.algebra.Plugin
import pavlosgi.freecli.config.dsl.ConfigDsl
import pavlosgi.freecli.config.parser.Parser
import pavlosgi.freecli.formatting._

trait Operations
  extends help.Operations
  with parser.Operations
  with dsl.Operations {

  def parseOrExit[A, G[_]: Plugin]
                 (args: Seq[String])
                 (config: ConfigDsl[G, A])
                 (implicit ev: G ~> Show, ev2: G ~> Parser): A = {

    parseConfig(args)(config) match {
      case Validated.Invalid(e) =>
        val errors =
          s"""
             |${usage(config)}
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
