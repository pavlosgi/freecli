package pavlosgi.freecli.core.interpreters.parser.command

import pavlosgi.freecli.core.api.command._
import pavlosgi.freecli.core.dsl.command._
import pavlosgi.freecli.core.dsl.config._
import pavlosgi.freecli.core.interpreters.parser.config._
import pavlosgi.freecli.testkit.Test

class CommandParserTest extends Test {
  case class Config(host: String, port: Int)
  case class SubConfig(dbName: String, dbPort: Int)
  case class ParentSubConfig(parent: Config, sub: SubConfig)

  describe("Parser") {
    it("parse command without config") {
      var hasRun = false
      parseCommand(Seq("command"))(
        cmd("command") {
          runs(hasRun = true)
        }).valid.run

      hasRun shouldBe true
    }

    it("fail to parse command without config if not in args") {
      val res =
        parseCommand(Seq("command2"))(
          cmd("command") {
            runs(())
          })

      res.invalid.toList.collect {
        case c: CommandNotFound => c.getClass.getName
      }.distinct.size should === (1)
    }

    it("parse command with config") {
      case class Config(host: String, port: Int)
      var conf = Option.empty[Config]

      parseCommand(Seq("command", "--host", "localhost", "--port", "8080"))(
        cmd("command") {
          config[Config](string --"host" :: int --"port") ::
          runs[Config](c => conf = Some(c))
        }).valid.run

      conf.get should === (Config("localhost", 8080))
    }

    it("fail to parse command with config due to command failure") {
      var conf = Option.empty[Config]

      val res =
        parseCommand(Seq("command2", "--host", "localhost", "--port", "8080"))(
          cmd("command") {
            config[Config](string --"host" :: int --"port") ::
            runs[Config](c => conf = Some(c))
          })

      res.invalid.toList.collect {
        case c: CommandNotFound => c.getClass.getName
      }.distinct.size should === (1)
    }

    it("fail to parse command with config due to config failure") {
      var conf = Option.empty[Config]

      val res =
        parseCommand(Seq("command", "--host", "localhost"))(
          cmd("command") {
            config[Config](string --"host" :: int --"port") ::
            runs[Config](c => conf = Some(c))
          })

      res.invalid.toList.collect {
        case c: FailedToParseConfig => c.getClass.getName
      }.distinct.size should === (1)
    }

    it("parse command with subcommand") {
      var hasRun = false

      parseCommand(Seq("command", "subcommand"))(
        cmd("command") {
          cmd("subcommand") {
            runs(hasRun = true)
          }
        }).valid.run

      hasRun shouldBe true
    }

    it("fail to parse command with subcommand") {
      val res =
        parseCommand(Seq("command"))(
          cmd("command") {
            cmd("subcommand") {
              runs(())
            }
          })

      res.invalid.toList should contain theSameElementsAs
        Seq(CommandNotFound(CommandField(CommandFieldName("subcommand"), None)))
    }

    it("parse command with subcommand that has config") {
      var conf = Option.empty[Config]

      parseCommand(Seq("command", "subcommand", "--host", "localhost", "--port", "8080"))(
        cmd("command") {
          cmd("subcommand") {
            config[Config](string --"host" :: int --"port") ::
            runs[Config](c => conf = Some(c))
          }
        }).valid.run

      conf.get should === (Config("localhost", 8080))
    }

    it("fail to parse command with subcommand that has config") {
      var conf = Option.empty[Config]
      val res =
        parseCommand(Seq("command", "subcommand", "--host", "localhost"))(
          cmd("command") {
            cmd("subcommand") {
              config[Config](string --"host" :: int --"port") ::
              runs[Config](c => conf = Some(c))
            }
          })

      res.invalid.toList.collect {
        case c: FailedToParseConfig => c.getClass.getName
      }.distinct.size should === (1)
    }

    it("fail to parse command with subcommand that has config due to subcommand missing") {
      var conf = Option.empty[Config]
      val res =
        parseCommand(Seq("command"))(
          cmd("command") {
            cmd("subcommand") {
              config[Config](string --"host" :: int --"port") ::
              runs[Config](c => conf = Some(c))
            }
          })

      res.invalid.toList should contain theSameElementsAs
        Seq(CommandNotFound(CommandField(CommandFieldName("subcommand"), None)))
    }

    it("parse command that has config with subcommand that has config") {
      var conf = Option.empty[ParentSubConfig]

      parseCommand(Seq("command", "--host", "localhost", "--port", "8080", "subcommand", "--dbName", "mydb", "--dbPort", "5432"))(
        cmd("command") {
          config[Config](string --"host" :: int --"port") ::
          cmd("subcommand") {
            config[SubConfig](string --"dbName" :: int --"dbPort") ::
            runs[ParentSubConfig](c => conf = Some(c))
          }
        }).valid.run

      conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
    }

    it("fail to parse command that has config with subcommand that has config") {
      var conf = Option.empty[ParentSubConfig]

      val res =
        parseCommand(Seq("command", "--dbName", "mydb", "--dbPort", "5432", "subcommand", "--host", "localhost", "--port", "8080"))(
          cmd("command") {
            config[Config](string --"host" :: int --"port") ::
            cmd("subcommand") {
              config[SubConfig](string --"dbName" :: int --"dbPort") ::
              runs[ParentSubConfig](c => conf = Some(c))
            }
          })

      res.invalid.toList.collect {
        case c: FailedToParseConfig => c.getClass.getName
      }.distinct.size should === (1)
    }

    it("parse command that has config with subcommand that has config if same config") {
      var conf = Option.empty[(String, String)]

      parseCommand(Seq("command", "--host", "localhost1", "subcommand", "--host", "localhost2"))(
        cmd("command") {
          config[String](string --"host") ::
          cmd("subcommand") {
            config[String](string --"host") ::
            runs[(String, String)](c => conf = Some(c))
          }
        }).valid.run

      conf.get should === ("localhost1" -> "localhost2")
    }

    it("parse command with multiple subcommands run first") {
      var conf = Option.empty[ParentSubConfig]

      parseCommand(Seq(
        "command",
        "--host",
        "localhost",
        "--port",
        "8080",
        "subcommand1",
        "--dbName1",
        "mydb",
        "--dbPort1",
        "5432"))(
        cmd("command") {
          config[Config](string --"host" :: int --"port") ::
          cmd("subcommand1") {
            config[SubConfig](string --"dbName1" :: int --"dbPort1") ::
            runs[ParentSubConfig](c => conf = Some(c))
          } ::
          cmd("subcommand2") {
            config[SubConfig](string --"dbName2" :: int --"dbPort2") ::
            runs[ParentSubConfig](c => conf = Some(c))
          } ::
          cmd("subcommand3") {
            config[SubConfig](string --"dbName3" :: int --"dbPort3") ::
            runs[ParentSubConfig](c => conf = Some(c))
          }
        }).valid.run

      conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
    }

    it("parse command with multiple subcommands run third") {
      var conf = Option.empty[ParentSubConfig]

      parseCommand(Seq(
        "command",
        "--host",
        "localhost",
        "--port",
        "8080",
        "subcommand3",
        "--dbName3",
        "mydb",
        "--dbPort3",
        "5432"))(
        cmd("command") {
          config[Config](string --"host" :: int --"port") ::
          cmd("subcommand1") {
            config[SubConfig](string --"dbName1" :: int --"dbPort1") ::
            runs[ParentSubConfig](c => conf = Some(c))
          } ::
          cmd("subcommand2") {
            config[SubConfig](string --"dbName2" :: int --"dbPort2") ::
            runs[ParentSubConfig](c => conf = Some(c))
          } ::
          cmd("subcommand3") {
            config[SubConfig](string --"dbName3" :: int --"dbPort3") ::
            runs[ParentSubConfig](c => conf = Some(c))
          }
        }).valid.run

      conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
    }

    it("fail to parse command with multiple subcommands if multiple commands match") {
      var conf = Option.empty[ParentSubConfig]

      val res =
        parseCommand(Seq(
          "command",
          "--host",
          "localhost",
          "--port",
          "8080",
          "subcommand1",
          "--dbName1",
          "mydb",
          "--dbPort1",
          "5432",
          "subcommand3",
          "--dbName3",
          "mydb",
          "--dbPort3",
          "5432"))(
          cmd("command") {
            config[Config](string --"host" :: int --"port") ::
            cmd("subcommand1") {
              config[SubConfig](string --"dbName1" :: int --"dbPort1") ::
              runs[ParentSubConfig](c => conf = Some(c))
            } ::
            cmd("subcommand2") {
              config[SubConfig](string --"dbName2" :: int --"dbPort2") ::
              runs[ParentSubConfig](c => conf = Some(c))
            } ::
            cmd("subcommand3") {
              config[SubConfig](string --"dbName3" :: int --"dbPort3") ::
              runs[ParentSubConfig](c => conf = Some(c))
            }
          })

      res.invalid
    }

    it("fail to parse command with multiple subcommands if subcommands have the same name") {
      var conf = Option.empty[ParentSubConfig]

      val res =
        parseCommand(Seq(
          "command",
          "--host",
          "localhost",
          "--port",
          "8080",
          "subcommand",
          "--dbName",
          "mydb",
          "--dbPort",
          "5432",
          "subcommand",
          "--dbName",
          "mydb",
          "--dbPort",
          "5432"))(
          cmd("command") {
            config[Config](string --"host" :: int --"port") ::
            cmd("subcommand") {
              config[SubConfig](string --"dbName1" :: int --"dbPort") ::
              runs[ParentSubConfig](c => conf = Some(c))
            } ::
            cmd("subcommand2") {
              config[SubConfig](string --"dbName2" :: int --"dbPort2") ::
              runs[ParentSubConfig](c => conf = Some(c))
            } ::
            cmd("subcommand3") {
              config[SubConfig](string --"dbName3" :: int --"dbPort3") ::
              runs[ParentSubConfig](c => conf = Some(c))
            }
          })

      res.invalid
    }
  }

}
