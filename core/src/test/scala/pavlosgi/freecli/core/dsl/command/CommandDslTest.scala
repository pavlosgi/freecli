package pavlosgi.freecli.core.dsl.command

import shapeless.test.illTyped

import pavlosgi.freecli.core.api.command.Command
import pavlosgi.freecli.core.dsl.config._
import pavlosgi.freecli.testkit.Test

class CommandDslTest extends Test {
  describe("ConfigDsl tests") {

    it("compiles for command with run") {
      cmd("command1", "some command") {
        runs(())
      }: CommandDsl[Command]
    }

    it("compiles for command with config and run with same type") {
      cmd("command1") {
        config[String](string --"field") ::
        runs[String](s => ())
      }
    }

    it("does not compile for command with config and run with same type reversed") {
      illTyped("""
        cmd("command1") {
          runs[String](s => ()) ::
          config[String](string --"field")
        }
      """)
    }

    it("compiles for command with config and run with different type") {
      case class A(s: String)

      cmd("command1") {
        config[A](string --"field" -'f') ::
        runs[A](s => ())
      }: CommandDsl[Command]
    }

    it("does not compile for command with config and run with different type reversed") {
      case class A(s: String)
      illTyped("""
        cmd("command1") {
          runs[A](s => ()) ::
          config[String](string --"field")
        }: CommandDsl[Command]
      """)
    }

    it("compiles for command with subcommands") {
      cmd("command1") {
        cmd("subcommand1") {
          config[String](string -- "second") ::
          runs[String](s => ())
        }
      }: CommandDsl[Command]
    }

    it("does not compile for command with config subcommands if parent has no config") {
      case class A(parentString: String, subString: String)
      illTyped("""
        cmd("command1") {
          subcommands[String] { parent =>
            cmd("subcommand1") {
              config[String](parent :: string -- "second") ::
              runs[A](s => ())
            }
          }
        }: CommandDsl[Command]""")
    }

    it("compiles for command with multiple subcommands") {
      cmd("command1") {
        cmd("subcommand1") {
          config[String](string -- "first") ::
          runs[String](s => ())
        } ::
        cmd("subcommand2") {
          config[String](string -- "second") ::
          runs[String](s => ())
        } ::
        cmd("subcommand3") {
          config[String](string -- "third") ::
          runs[String](s => ())
        }
      }: CommandDsl[Command]
    }

    it("compiles for command with config and subcommands") {
      case class A(parentString: String, subString: String)
      cmd("command1") {
        config[String](string --"field") ::
        cmd("subcommand1") {
          config[String](string -- "second") ::
          runs[A](s => ())
        }
      }: CommandDsl[Command]
    }

    it("does not compile for command with config and subcommands without parent config") {
      illTyped("""
        cmd("command1") {
          config[String](string --"field") ::
          cmd("subcommand1") {
            config[String](string -- "second") ::
            runs[String](s => ())
          }
        }: CommandDsl[Command]""")
    }

    it("compiles for command with config and multiple subcommands") {
      case class A(parentString: String, subString: String)
      cmd("command1") {
        config[String](string --"field") ::
        cmd("subcommand1") {
          config[String](string -- "first") ::
          runs[A](s => ())
        } ::
        cmd("subcommand2") {
          config[String](string -- "second") ::
          runs[A](s => ())
        } ::
        cmd("subcommand3") {
          config[String](string -- "third") ::
          runs[A](s => ())
        }
      }: CommandDsl[Command]
    }

    it("compiles for command with config and multiple subcommands of N depth") {
      case class A(parentString: String, subString: String)
      case class B(parent1: String, parent2: String, parent3: String, subString: String)

      cmd("command1") {
        config[String](string --"field") ::
        cmd("subcommand1") {
          config[String](string --"subfield1") ::
          cmd("subcommand2") {
            config[String](string -- "subfield2") ::
            cmd("subcommand3") {
              config[String](string -- "subfield3") ::
              runs[B](s => ())
            }
          }
        } ::
        cmd("subcommand4") {
          config[String](string -- "subfield4") ::
          runs[A](s => ())
        } ::
        cmd("subcommand5") {
          config[String](string -- "subfield5") ::
          runs[A](s => ())
        }
      }: CommandDsl[Command]
    }

    it("compiles for command with config and multiple subcommands complex tree") {
      case class A(b: Int, c: Boolean)
      case class B(parent1: A, parent2: String, subString: String)
      case class C(s: String)
      case class D(parent1: A, sub: C)

      cmd("command1") {
        config[A] {
          int     --"intField"  ::
          boolean --"boolField"
        } ::
        cmd("subcommand1") {
          config[String](string --"subfield1") ::
          cmd("subcommand2") {
            cmd("subcommand3") {
              config[String](string --"subfield3") ::
              runs[B](s => ())
            }
          }
        } ::
        cmd("subcommand4") {
          config[String](string -- "subfield4") ::
          cmd("subcommand5") {
            runs[(A, String)](s => ())
          } ::
          cmd("subcommand6") {
            config[Int](int --"subfield6") ::
            runs[(A, String, Int)](s => ())
          }
        } ::
        cmd("subcommand7") {
          config[String](string --"subfield7") ::
          runs[(A, String)](s => ())
        } ::
        cmd("subcommand8") {
          config[C](string --"subfield8") ::
          runs[D](s => ())
        }
      }: CommandDsl[Command]
    }

  }
}
