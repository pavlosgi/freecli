package freecli
package command

import command.all._
import config.all._
import command.api._
import command.parser._
import testkit.Test
import Helpers._
import config.api.OptionAction
import option.api.{HelpAction, VersionAction}

class CommandParserTest extends Test {
  describe("Command Parser") {
    describe("Single command") {
      it("parse command without config") {
        var hasRun = false
        parseCommand(
          cmd("command") {
            runs({hasRun = true})
          }).run(Seq("command"))._2.success.run

        hasRun shouldBe true
      }

      it("fail to parse command without config if not in args") {
        val (_, res) =
          parseCommand(
            cmd("command") {
              runs(())
            }).run(Seq("command2"))

        res.failure should matchPattern {
          case
            OtherCommandErrors(
              Some(AdditionalArgumentsFound(List(arg))),
              None,
              List(_),
              None,
              None)

            if arg === "command2" =>
        }
      }

      it("parse command with config") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        parseCommand(
          cmd("command") {
            takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
            runs[Config](c => conf = Some(c))
          }).run(Seq("command", "--host", "localhost", "--port", "8080"))._2.success.run

        conf.get should === (Config("localhost", 8080))
      }

      it("fail to parse command with config due to command failure") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        val (_, res) =
          parseCommand(
            cmd("command") {
              takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
              runs[Config](c => conf = Some(c))
            }).run(Seq("command2", "--host", "localhost", "--port", "8080"))

        res.failure should matchPattern {
          case
            OtherCommandErrors(
              Some(AdditionalArgumentsFound(args)),
              None,
              List(CommandNotFound(CommandField(n, _))),
              None,
              None)

            if args.diff(List("command2", "--host", "localhost", "--port", "8080")).isEmpty &&
            n.name === "command" =>
        }
      }

      it("fail to parse command with config due to config failure") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        val (_, res) =
          parseCommand(
            cmd("command") {
              takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
              runs[Config](c => conf = Some(c))
            }).run(Seq("command", "--host", "localhost", "file1"))

        res.failure should matchPattern {
          case OtherCommandErrors(Some(_), Some(_), Nil, None, None) =>
        }
      }

      it("parse and run command with help") {
        var hasRun = false

        parseCommand(
          cmd("command") {
            takes(O.help --"help") ::
            runs({hasRun = true})
          }).run(Seq("command"))._2.success.run

        hasRun should === (true)
      }

      it("parse command with help and display help") {
        val action = parseCommand(
          cmd("command") {
            takes(O.help --"help") ::
            runs(())
          }).run(Seq("command", "--help"))._2.action

        Option(action).collect {
          case ConfigAction(_, _, OptionAction(HelpAction)) => true
        }.exists(identity) should === (true)
      }

      it("parse and run command with version") {
        var hasRun = false

        parseCommand(
          cmd("command") {
            takes(O.version --"version" -~ O.value("v1.0")) ::
            runs({hasRun = true})
          }).run(Seq("command"))._2.success.run

        hasRun should === (true)
      }

      it("parse command with version and display version") {
        val action = parseCommand(
          cmd("command") {
            takes(O.version --"version" -~ O.value("v2.0")) ::
            runs(())
          }).run(Seq("command", "--version"))._2.action

        Option(action).collect {
          case ConfigAction(_, _, OptionAction(VersionAction(_))) => true
        }.exists(identity) should === (true)
      }
    }

    describe("Command with single subcommand") {
      it("parse command with subcommand") {
        var hasRun = false

        parseCommand(
          cmd("command") {
            cmd("subcommand") {
              runs({hasRun = true})
            }
          }).run(Seq("command", "subcommand"))._2.success.run

        hasRun shouldBe true
      }

      it("fail to parse command with subcommand") {
        val (_, res) =
          parseCommand(
            cmd("command") {
              cmd("subcommand") {
                runs(())
              }
            }).run(Seq("command"))

        res.failure should matchPattern {
          case
            ParentCommandError(
              CommandField(parent, _),
              OtherCommandErrors(
                None,
                None,
                List(CommandNotFound(
                  CommandField(fname, None))),

                None,
                None))

            if parent.name === "command" && fname.name === "subcommand" =>
        }
      }

      it("parse command with subcommand that has config from options") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        parseCommand(
          cmd("command") {
            cmd("subcommand") {
              takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
              runs[Config](c => conf = Some(c))
            }
          }).run(Seq("command", "subcommand", "--host", "localhost", "--port", "8080"))._2.success.run

        conf.get should === (Config("localhost", 8080))
      }

      it("parse command with subcommand that has config from arguments") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        parseCommand(
          cmd("command") {
            cmd("subcommand") {
              takesG[Config](string -~ name("arg1") :: int) ::
              runs[Config](c => conf = Some(c))
            }
          }).run(Seq("command", "subcommand", "localhost", "8080"))._2.success.run

        conf.get should === (Config("localhost", 8080))
      }

      it("parse command with subcommand that has config from options and arguments") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]

        parseCommand(
          cmd("command") {
            cmd("subcommand") {
              takesG[Config](O.string --"host" -~ req :: int) ::
              runs[Config](c => conf = Some(c))
            }
          }).run(Seq("command", "subcommand", "--host", "localhost", "8080"))._2.success.run

        conf.get should === (Config("localhost", 8080))
      }

      it("fail to parse command with subcommand that has config") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]
        val (_, res) =
          parseCommand(
            cmd("command") {
              cmd("subcommand") {
                takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
                runs[Config](c => conf = Some(c))
              }
            }).run(Seq("command", "subcommand", "--host", "localhost"))

        res.failure should matchPattern {
          case
            ParentCommandError(
              CommandField(parent, None),
              OtherCommandErrors(
                None,
                Some(FailedToParseConfig(CommandField(sub, None), _)),
                Nil,
                None,
                None))

            if parent.name === "command" && sub.name === "subcommand" =>
        }
      }

      it("fail to parse command with subcommand that has config due to subcommand missing") {
        case class Config(host: String, port: Int)
        var conf = Option.empty[Config]
        val (_, res) =
          parseCommand(
            cmd("command") {
              cmd("subcommand") {
                takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
                runs[Config](c => conf = Some(c))
              }
            }).run(Seq("command"))

        res.failure should matchPattern {
          case
            ParentCommandError(
              CommandField(parent, None),
              OtherCommandErrors(
                None,
                None,
                List(CommandNotFound(
                  CommandField(fname, None))),

                None,
                None))

            if parent.name === "command" && fname.name === "subcommand" =>
        }
      }

      it("parse command that has config with subcommand that has config") {
        case class Config(host: String, port: Int)
        case class SubConfig(dbName: String, dbPort: Int)
        case class ParentSubConfig(parent: Config, sub: SubConfig)

        var conf = Option.empty[ParentSubConfig]

        parseCommand(
          cmd("command") {
            takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
            cmd("subcommand") {
              takesG[SubConfig](O.string --"dbName" -~ req :: O.int --"dbPort"-~ req) ::
              runs[ParentSubConfig](c => conf = Some(c))
            }
          }).run(Seq(
          "command",
          "--host",
          "localhost",
          "--port",
          "8080",
          "subcommand",
          "--dbName",
          "mydb",
          "--dbPort",
          "5432"))._2.success.run

        conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
      }

      it("fail to parse command that has config with subcommand that has config if arguments for config are parsed in the wrong order") {
        case class Config(host: String, port: Int)
        case class SubConfig(dbName: String, dbPort: Int)
        case class ParentSubConfig(parent: Config, sub: SubConfig)

        var conf = Option.empty[ParentSubConfig]

        val (_, res) =
          parseCommand(
            cmd("command") {
              takesG[Config](O.string --"host" -~ req :: O.int --"port" -~ req) ::
              cmd("subcommand") {
                takesG[SubConfig](O.string --"dbName" -~ req :: O.int --"dbPort" -~ req) ::
                runs[ParentSubConfig](c => conf = Some(c))
              }
            }).run(Seq("command",
            "--dbName",
            "mydb",
            "--dbPort",
            "5432",
            "subcommand",
            "--host",
            "localhost",
            "--port",
            "8080"))

        res.failure should matchPattern {
          case
            OtherCommandErrors(
              Some(AdditionalArgumentsFound(args)),
              Some(FailedToParseConfig(CommandField(field, None), _)),
              Nil,
              None,
              None)

            if args === List(
                "--dbName",
                "mydb",
                "--dbPort",
                "5432",
                "subcommand") &&

              field.name === "command" =>
        }
      }

      it("parse command that has config with subcommand that has config if same config") {
        var conf = Option.empty[(String, String)]

        parseCommand(
          cmd("command") {
            takesG[String](O.string --"host"-~ req) ::
            cmd("subcommand") {
              takesG[String](O.string --"host"-~ req) ::
              runs[(String, String)](c => conf = Some(c))
            }
        }).run(Seq("command", "--host", "localhost1", "subcommand", "--host", "localhost2"))._2.success.run

        conf.get should === ("localhost1" -> "localhost2")
      }

      it("parse command and run subcommand with help") {
        var hasRun = false

        parseCommand(
          cmd("command") {
            takes(O.help --"help") ::
            cmd("subcommand") {
              takes(O.help --"help") ::
              runs({hasRun = true})
            }
          }).run(Seq("command", "subcommand"))._2.success.run

        hasRun should === (true)
      }

      it("parse command and subcommand and display help") {
        val action = parseCommand(
          cmd("command") {
            takes(O.help --"help") ::
            cmd("subcommand") {
              takes(O.help --"help") ::
              runs(())
            }
          }).run(Seq("command", "subcommand", "--help"))._2.action

        Option(action).collect {
          case ConfigAction(_, _, OptionAction(HelpAction)) => true
        }.exists(identity) should === (true)
      }

      it("parse command and run subcommand with version") {
        var hasRun = false

        parseCommand(
          cmd("command") {
            takes(O.version --"version" -~ O.value("v1.0")) ::
            cmd("subcommand") {
              takes(O.version --"version" -~ O.value("v2.0")) ::
              runs({hasRun = true})
            }
          }).run(Seq("command", "subcommand"))._2.success.run

        hasRun should === (true)
      }

      it("parse command and subcommand and display version") {
        val action = parseCommand(
          cmd("command") {
            takes(O.version --"version" -~ O.value("v1")) ::
            cmd("subcommand") {
              takes(O.version --"version" -~ O.value("v2")) ::
              runs(())
            }
          }).run(Seq("command", "subcommand", "--version"))._2.action

        Option(action).collect {
          case ConfigAction(_, _, OptionAction(VersionAction(_))) => true
        }.exists(identity) should === (true)
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

        parseCommand(dsl).run(Seq(
          "command",
          "--host",
          "localhost",
          "--port",
          "8080",
          "subcommand1",
          "--dbName1",
          "mydb",
          "--dbPort1",
          "5432"))._2.success.run

        conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
      }

      it("parse command with multiple subcommands (run third)") {
        conf = Option.empty[ParentSubConfig]

        parseCommand(dsl).run(Seq(
          "command",
          "--host",
          "localhost",
          "--port",
          "8080",
          "subcommand3",
          "--dbName3",
          "mydb",
          "--dbPort3",
          "5432"))._2.success.run

        conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
      }

      it("fail to parse command with multiple subcommands if multiple commands match") {
        val (_, res) =
          parseCommand(dsl).run(Seq(
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
            "5432"))

        res.failure
      }

      it("fail to parse command with multiple subcommands if subcommands have the same name") {
        val (_, res) =
          parseCommand(dsl).run(Seq(
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
            "5432"))

        res.failure
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

        parseCommand(dsl).run(Seq(
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
          "5432"))._2.success.run

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

        parseCommand(dsl).run(Seq(
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
          "5432"))._2.success.run

        conf.get should === (ParentSubConfig(Config("localhost", 8080), SubConfig("mydb", 5432)))
      }
    }
  }
}
