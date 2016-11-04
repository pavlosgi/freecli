package pavlosgi.freecli.core.config.interpreters

import cats.syntax.all._

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config._
import pavlosgi.freecli.core.config.interpreters.help._
import pavlosgi.freecli.testkit.Test

class HelpTest extends Test {
  describe("Help") {
    it("show help") {
      case class PGConfig(host: String, port: Int, debug: Boolean)
      case class ServerConfig(host: String, port: Int, pgConfig: PGConfig)

      val dsl =
        config[ServerConfig] {
          string --"host" -'h' -?"Server host" ::
          int    --"port" -?"Server port"      ::
          sub[PGConfig]("PostgreSQL configuration") {
            string --"pg-host" -'g' -?"PostgreSQL host" ::
            int    --"pg-port" -'p' -?"PostgreSQL port" ::
            flag   -'d'
          }
        }

      val help = showHelp(dsl)
      print(help)

      Seq(
        FieldName("host").show,
        FieldAbbreviation('h').show,
        Description("Server host").show,
        FieldName("port").show,
        Description("PostgreSQL configuration").show,
        FieldName("pg-host").show,
        FieldAbbreviation('g').show,
        Description("PostgreSQL host").show,
        FieldName("pg-port").show,
        FieldAbbreviation('p').show,
        Description("PostgreSQL port").show,
        FieldAbbreviation('d').show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
