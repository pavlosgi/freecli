package pavlosgi.freecli.command

import pavlosgi.freecli.command.all._
import pavlosgi.freecli.config.all._
import pavlosgi.freecli.command.api._
import pavlosgi.freecli.command.parser._
import pavlosgi.freecli.testkit.Test
import pavlosgi.freecli.Helpers._

class CommandParserTest extends Test {
  describe("Command Parser") {
    describe("Single command") {
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

        res.invalid.errors.toList.collect {
          case c: CommandNotFound => c.getClass.getName
        }.distinct.size should === (1)
      }

      it("parse command with config") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        parseCommand(Seq("command", "--host", "localhost", "--port", "8080"))(
          cmd("command") {
            takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
            runs[Config](c => conf = Some(c))
          }).valid.run

        conf.get should === (Config("localhost", 8080))
      }

      it("fail to parse command with config due to command failure") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        val res =
          parseCommand(Seq("command2", "--host", "localhost", "--port", "8080"))(
            cmd("command") {
              takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
              runs[Config](c => conf = Some(c))
            })

        res.invalid.errors.toList.collect {
          case c: CommandNotFound => c.getClass.getName
        }.distinct.size should === (1)
      }

      it("fail to parse command with config due to config failure") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        val res =
          parseCommand(Seq("command", "--host", "localhost", "file1"))(
            cmd("command") {
              takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
              runs[Config](c => conf = Some(c))
            })

        res.invalid.errors.toList.collect {
          case c: FailedToParseConfig => c.getClass.getName
        }.distinct.size should === (1)
      }
    }

    describe("Command with single subcommand") {
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

        res.invalid.errors.toList should contain theSameElementsAs
          Seq(CommandNotFound(CommandField(CommandFieldName("subcommand"), None)))
      }

      it("parse command with subcommand that has config from options") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        parseCommand(Seq("command", "subcommand", "--host", "localhost", "--port", "8080"))(
          cmd("command") {
            cmd("subcommand") {
              takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
              runs[Config](c => conf = Some(c))
            }
          }).valid.run

        conf.get should === (Config("localhost", 8080))
      }

      it("parse command with subcommand that has config from arguments") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        parseCommand(Seq("command", "subcommand", "localhost", "8080"))(
          cmd("command") {
            cmd("subcommand") {
              takesG[Config](string -~ name("arg1") :: int) ::
              runs[Config](c => conf = Some(c))
            }
          }).valid.run

        conf.get should === (Config("localhost", 8080))
      }

      it("parse command with subcommand that has config from options and arguments") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        parseCommand(Seq("command", "subcommand", "--host", "localhost", "8080"))(
          cmd("command") {
            cmd("subcommand") {
              takesG[Config](O.string --"host" -~ req :: int) ::
              runs[Config](c => conf = Some(c))
            }
          }).valid.run

        conf.get should === (Config("localhost", 8080))
      }

      it("fail to parse command with subcommand that has config") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]
        val res =
          parseCommand(Seq("command", "subcommand", "--host", "localhost"))(
            cmd("command") {
              cmd("subcommand") {
                takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
                runs[Config](c => conf = Some(c))
              }
            })

        res.invalid.errors.toList.collect {
          case c: FailedToParseConfig => c.getClass.getName
        }.distinct.size should === (1)
      }

      it("fail to parse command with subcommand that has config due to subcommand missing") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]
        val res =
          parseCommand(Seq("command"))(
            cmd("command") {
              cmd("subcommand") {
                takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
                runs[Config](c => conf = Some(c))
              }
            })

        res.invalid.errors.toList should contain theSameElementsAs
          Seq(CommandNotFound(CommandField(CommandFieldName("subcommand"), None)))
      }

      it("parse command that has config with subcommand that has config") {
        case class Config(host: String, port: Int)
        case class SubConfig(dbName: String, dbPort: Int)
        case class ParentSubConfig(parent: Config, sub: SubConfig)

        var conf = Option.empty[ParentSubConfig]

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
          "5432"))(

          cmd("command") {
            takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
            cmd("subcommand") {
              takesG[SubConfig](O.string --"dbName" -~ req :: O.int --"dbPort"-~ req) ::
              runs[ParentSubConfig](c => conf = Some(c))
            }
          }).valid.run

        conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
      }

      it("fail to parse command that has config with subcommand that has config if arguments for config are parse in the wrong order") {
        case class Config(host: String, port: Int)
        case class SubConfig(dbName: String, dbPort: Int)
        case class ParentSubConfig(parent: Config, sub: SubConfig)

        var conf = Option.empty[ParentSubConfig]

        val res =
          parseCommand(Seq("command",
            "--dbName",
            "mydb",
            "--dbPort",
            "5432",
            "subcommand",
            "--host",
            "localhost",
            "--port",
            "8080"))(

            cmd("command") {
              takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
              cmd("subcommand") {
                takesG[SubConfig](O.string --"dbName" -~ req :: O.int --"dbPort" -~ req) ::
                runs[ParentSubConfig](c => conf = Some(c))
              }
            })

        res.invalid.errors.toList.collect {
          case c: AdditionalArgumentsFound => c.getClass.getName
          case c: CommandNotFound => c.getClass.getName
        }.distinct.size should === (2)
      }

      it("parse command that has config with subcommand that has config if same config") {
        var conf = Option.empty[(String, String)]

        parseCommand(Seq("command", "--host", "localhost1", "subcommand", "--host", "localhost2"))(
          cmd("command") {
            takesG[String](O.string --"host"-~ req) ::
            cmd("subcommand") {
              takesG[String](O.string --"host"-~ req) ::
              runs[(String, String)](c => conf = Some(c))
            }
        }).valid.run

        conf.get should === ("localhost1" -> "localhost2")
      }
    }

    describe("Multiple subcommands distinct names") {
      case class Config(host: String, port: Int)
      case class SubConfig(dbName: String, dbPort: Int)
      case class ParentSubConfig(parent: Config, sub: SubConfig)

      var conf = Option.empty[ParentSubConfig]

      val dsl =
        cmd("command") {
          takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
          cmd("subcommand1") {
            takesG[SubConfig](O.string --"dbName1" -~ req :: O.int --"dbPort1"-~ req) ::
            runs[ParentSubConfig](c => conf = Some(c))
          } ::
          cmd("subcommand2") {
            takesG[SubConfig](O.string --"dbName2" -~ req :: O.int --"dbPort2"-~ req) ::
            runs[ParentSubConfig](c => conf = Some(c))
          } ::
          cmd("subcommand3") {
            takesG[SubConfig](O.string --"dbName3" -~ req :: O.int --"dbPort3"-~ req) ::
            runs[ParentSubConfig](c => conf = Some(c))
          }
        }

      it("parse command with multiple subcommands (run first)") {
        conf = Option.empty[ParentSubConfig]

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
          "5432"))(dsl).valid.run

        conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
      }

      it("parse command with multiple subcommands (run third)") {
        conf = Option.empty[ParentSubConfig]

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
          "5432"))(dsl).valid.run

        conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
      }

      it("fail to parse command with multiple subcommands if multiple commands match") {
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
            "5432"))(dsl)

        res.invalid
      }

      it("fail to parse command with multiple subcommands if subcommands have the same name") {
        val res =
          parseCommand(Seq(
            "command",
            "--host",
            "localhost",
            "--port",
            "8080",
            "subcommand",
            "--dbName1",
            "mydb",
            "--dbPort1",
            "5432",
            "subcommand",
            "--dbName1",
            "mydb",
            "--dbPort1",
            "5432"))(dsl)

        res.invalid
      }
    }

    describe("Multiple subcommands similar names") {
      case class Config(host: String, port: Int)
      case class SubConfig(dbName: String, dbPort: Int)
      case class ParentSubConfig(parent: Config, sub: SubConfig)

      var conf = Option.empty[ParentSubConfig]

      it("parse command with subcommands with the same name at different depths when parsed subcommand comes first") {
        conf = Option.empty[ParentSubConfig]
        val dsl =
          cmd("command") {
            takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
            cmd("subcommand1") {
              cmd("subcommand2") {
                takesG[SubConfig](O.string --"dbName1" -~ req :: O.int --"dbPort1"-~ req) ::
                runs[ParentSubConfig](c => conf = Some(c))
              }
            } ::
            cmd("subcommand2") {
              takesG[SubConfig](O.string --"dbName2" -~ req :: O.int --"dbPort2"-~ req) ::
              runs[ParentSubConfig](c => conf = Some(c))
            } ::
            cmd("subcommand3") {
              takesG[SubConfig](O.string --"dbName3" -~ req :: O.int --"dbPort3"-~ req) ::
              runs[ParentSubConfig](c => conf = Some(c))
            }
          }

        val res =
          parseCommand(Seq(
            "command",
            "--host",
            "localhost",
            "--port",
            "8080",
            "subcommand1",
            "subcommand2",
            "--dbName1",
            "mydb",
            "--dbPort1",
            "5432"))(dsl).valid.run

        conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
      }

      it("parse command with subcommands with the same name at different depths when parse subcommand comes second") {
        conf = Option.empty[ParentSubConfig]
        val dsl =
          cmd("command") {
            takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
            cmd("subcommand2") {
              takesG[SubConfig](O.string --"dbName2" -~ req :: O.int --"dbPort2"-~ req) ::
              runs[ParentSubConfig](c => conf = Some(c))
            } ::
            cmd("subcommand1") {
              cmd("subcommand2") {
                takesG[SubConfig](O.string --"dbName1" -~ req :: O.int --"dbPort1"-~ req) ::
                runs[ParentSubConfig](c => conf = Some(c))
              }
            } ::
            cmd("subcommand3") {
              takesG[SubConfig](O.string --"dbName3" -~ req :: O.int --"dbPort3"-~ req) ::
              runs[ParentSubConfig](c => conf = Some(c))
            }
          }

        val res =
          parseCommand(Seq(
            "command",
            "--host",
            "localhost",
            "--port",
            "8080",
            "subcommand1",
            "subcommand2",
            "--dbName1",
            "mydb",
            "--dbPort1",
            "5432"))(dsl).valid.run

        conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
      }
    }
  }
}
