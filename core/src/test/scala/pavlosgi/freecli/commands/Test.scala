package pavlosgi
package freecli
package commands

import shapeless._

import pavlosgi.freecli.commands.all._
import pavlosgi.freecli.config.all._

class Test extends testkit.Test {

  describe("Parser") {
    it("parse command") {
      commands.all.parse(Seq("my-command"))(cmd("my-command").dsl)
    }

    it("parse command with run") {
      var commandRun = false

      commands.all.parse(Seq("my-command"))(cmd("my-command") {
        commands.all.run(() => commandRun = true)
      }).valid.run

      commandRun should === (true)
    }

    it("parse command with config and run") {
      var commandRun = false
      var serverConfig = Option.empty[ServerConfig]

      commands.all.parse(Seq("my-command", "--host", "localhost", "--port", "5432"))(
        cmd("my-command") {
          config(getServerConfig) ~
          commands.all.run((s: ServerConfig) => {
            serverConfig = Some(s)
            commandRun = true
          })
        }).valid.run

      commandRun should === (true)
      serverConfig should === (Some(ServerConfig("localhost", 5432)))
    }

    it("parse command with single subcommand and run subcommand") {
      var subCommandRun = false

      commands.all.parse(Seq("my-command", "my-subcommand"))(cmd("my-command") {
        subcommand {
          cmd("my-subcommand") {
            commands.all.run(() => subCommandRun = true)
          }
        }
      }).valid.run

      subCommandRun should === (true)
    }

    it("parse command with single subcommand with config and run subcommand") {
      var subCommandRun = false
      var serverConfig = Option.empty[ServerConfig]

      commands.all.parse(Seq("my-command", "my-subcommand", "--host", "localhost", "--port", "5432"))(cmd("my-command") {
        subcommand {
          cmd("my-subcommand") {
            config(getServerConfig) ~
            commands.all.run((s: ServerConfig) => {
              serverConfig = Some(s)
              subCommandRun = true
            })
          }
        }
      }).valid.run

      subCommandRun should === (true)
      serverConfig.get should === (ServerConfig("localhost", 5432))
    }

    it("parse command with multiple subcommands and run subcommand") {
      var subCommandRun1 = false
      var subCommandRun2 = false
      var subCommandRun3 = false
      var subCommandRun4 = false
      var serverConfig1 = Option.empty[ServerConfig]
      var serverConfig2 = Option.empty[ServerConfig]
      var serverConfig3 = Option.empty[ServerConfig]
      var serverConfig4 = Option.empty[ServerConfig]

      val sub1 =
        cmd("my-subcommand1") {
            config(getServerConfig) ~
            commands.all.run((s: ServerConfig) => {
              serverConfig1 = Some(s)
              subCommandRun1 = true
            })
          }

      val sub2 =
        cmd("my-subcommand2") {
            config(getServerConfig) ~
            commands.all.run((s: ServerConfig) => {
              serverConfig2 = Some(s)
              subCommandRun2 = true
            })
          }

      val sub3 =
        cmd("my-subcommand3") {
            config(getServerConfig) ~
            commands.all.run((s: ServerConfig) => {
              serverConfig3 = Some(s)
              subCommandRun3 = true
            })
          }

      val sub4 =
        cmd("my-subcommand4") {
            config(getServerConfig) ~
            commands.all.run((s: ServerConfig) => {
              serverConfig4 = Some(s)
              subCommandRun4 = true
            })
          }

      commands.all.parse(
        Seq(
          "my-command",
          "my-subcommand3",
          "--host",
          "localhost",
          "--port",
          "5432"
        ))(
          cmd("my-command") {
            subcommand(sub1) ~
            subcommand(sub2) ~
            subcommand(sub3) ~
            subcommand(sub4)
          }).valid.run

      subCommandRun1 should === (false)
      subCommandRun2 should === (false)
      subCommandRun3 should === (true)
      subCommandRun4 should === (false)
      serverConfig1 should be (empty)
      serverConfig2 should be (empty)
      serverConfig3.get should === (ServerConfig("localhost", 5432))
      serverConfig4 should be (empty)
    }

    it("parse command with deeply nested subcommand and run subcommand") {
      var subCommandRun4 = false
      var subCommandRun5 = false
      var serverConfig4 = Option.empty[ServerConfig]
      var serverConfig5 = Option.empty[ServerConfig]

      commands.all.parse(
        Seq(
          "my-command",
          "my-subcommand1",
          "my-subcommand2",
          "my-subcommand3",
          "my-subcommand4",
          "--host",
          "localhost",
          "--port",
          "5432"
        ))(
          cmd("my-command") {
            subcommand(cmd("my-subcommand1") {
              subcommand(cmd("my-subcommand2") {
                subcommand(cmd("my-subcommand3") {
                  subcommand(cmd("my-subcommand4") {
                    config(getServerConfig) ~
                    commands.all.run((s: ServerConfig) => {
                      serverConfig4 = Some(s)
                      subCommandRun4 = true
                    })
                  })
                })
              })
            }) ~
            subcommand(cmd("my-subcommand5") {
               config(getServerConfig) ~
                  commands.all.run((s: ServerConfig) => {
                    serverConfig5 = Some(s)
                    subCommandRun5 = true
                  })
            })
          }).valid.run

      subCommandRun4 should === (true)
      subCommandRun5 should === (false)
      serverConfig4.get should === (ServerConfig("localhost", 5432))
      serverConfig5 should be (empty)
    }
  }

  case class ServerConfig(host: String, port: Int)

  def getServerConfig = {
    (string("host") |@| int("port")).map(ServerConfig.apply)
  }

}

