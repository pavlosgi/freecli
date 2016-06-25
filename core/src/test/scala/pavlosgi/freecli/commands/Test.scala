package pavlosgi
package freecli
package commands

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest._
import shapeless._

import pavlosgi.freecli.commands.all._
import pavlosgi.freecli.config.all._

class Test
  extends FunSpec
  with Matchers
  with TypeCheckedTripleEquals
  with Helpers {

  describe("Parser") {
    it("parse empty command") {
      parse[ParserShow](Seq("my-command"))(cmd("my-command").dsl)
    }

    it("parse empty command with run") {
      var commandRun = false

      parse[ParserShow](Seq("my-command"))(cmd("my-command") {
        commands.dsl.Operations.run(() => commandRun = true)
      }).valid.run

      commandRun should === (true)
    }

    it("parse empty command with config and run") {
      var commandRun = false
      var serverConfig = Option.empty[ServerConfig]

      parse[ParserShow](Seq("my-command", "--host", "localhost", "--port", "5432"))(
        cmd("my-command") {
          config(getServerConfig) ~
          commands.dsl.Operations.run((s: ServerConfig) => {
            serverConfig = Some(s)
            commandRun = true
          })
        }).valid.run

      commandRun should === (true)
      serverConfig should === (Some(ServerConfig("localhost", 5432)))
    }

    it("parse command with single subcommand and run subcommand") {
      var subCommandRun = false

      parse[ParserShow](Seq("my-command", "my-subcommand"))(cmd("my-command") {
        subcommand {
          cmd("my-subcommand") {
            commands.dsl.Operations.run(() => subCommandRun = true)
          }
        }
      }).valid.run

      subCommandRun should === (true)
    }

    it("parse command with single subcommand with config and run subcommand") {
      var subCommandRun = false
      var serverConfig = Option.empty[ServerConfig]

      parse[ParserShow](Seq("my-command", "my-subcommand", "--host", "localhost", "--port", "5432"))(cmd("my-command") {
        subcommand {
          cmd("my-subcommand") {
            config(getServerConfig) ~
            commands.dsl.Operations.run((s: ServerConfig) => {
              serverConfig = Some(s)
              subCommandRun = true
            })
          }
        }
      }).valid.run

      subCommandRun should === (true)
      serverConfig.get should === (ServerConfig("localhost", 5432))
    }
  }

  case class ServerConfig(host: String, port: Int)

  def getServerConfig = {
    (string("host") |@| int("port")).map(ServerConfig.apply)
  }

}

