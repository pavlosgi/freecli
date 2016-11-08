package pavlosgi.freecli.core.interpreters.help.config

import cats.syntax.all._

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config._
import pavlosgi.freecli.testkit.Test

class ConfigHelpTest extends Test {
  describe("Help") {
    it("show help") {
      case class PGConfig(host: String, port: Int, debug: Boolean)
      case class ServerConfig(host: String, port: Int, pgConfig: PGConfig)

      val dsl =
        config[ServerConfig] {
          string --"host" -'h' -~ des("Server host") ::
          int    --"port" -~ des("Server port")      ::
          sub[PGConfig]("PostgreSQL configuration") {
            string --"pg-host" -'g' -~ des("PostgreSQL host") ::
              (int    --"pg-port" -'p' -~ des("PostgreSQL port"): ConfigDsl[Int]) ::
            flag   -'d'
          }
        }

      val help = configHelp(dsl)
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
