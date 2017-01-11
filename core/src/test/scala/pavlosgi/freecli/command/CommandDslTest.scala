package pavlosgi.freecli.command

import shapeless.test.illTyped

import pavlosgi.freecli.command.api.Command
import pavlosgi.freecli.core.all._
import pavlosgi.freecli.command.all._
import pavlosgi.freecli.config.all._
import pavlosgi.freecli.testkit.Test

class CommandDslTest extends Test {
  describe("ConfigDsl tests") {

    it("compiles for command with run") {
      cmd("command1", des("some command")) {
        runs(())
      }: CommandDsl[Command]
    }

    it("compiles for command with config and run with same type") {
      cmd("command1") {
        takesG[String](O.string --"field" -~ req) ::
        runs[String](s => ())
      }: CommandDsl[Command]
    }

    it("does not compile for command with config and run with same type reversed") {
      illTyped("""
        cmd("command1") {
          runs[String](s => ()) ::
          takesG[String](O.string --"field" -~ req)
        }: CommandDsl[Command]
      """)
    }

    it("compiles for command with config and run with different type") {
      case class A(s: String)

      cmd("command1") {
        takesG[A](O.string --"field" -'f' -~ req) ::
        runs[A](_ => ())
      }: CommandDsl[Command]
    }

    it("does not compile for command with config and run with different type reversed") {
      case class A(s: String)
      illTyped("""
        cmd("command1") {
          runs[A](s => ()) ::
          takesG[String](O.string --"field" -~ req)
        }: CommandDsl[Command]
      """)
    }

    it("compiles for command with config and help") {
      cmd("command1") {
        takesG[Option[String]](O.string -'o' :: O.help -'h') ::
        runs[Option[String]](_ => ())
      }
    }

    it("compiles for command with help") {
      cmd("command1") {
        takes(O.help -'h') ::
        runs(())
      }
    }

    it("compiles for command with subcommands") {
      cmd("command1") {
        cmd("subcommand1") {
          takesG[String](O.string -- "second" -~ req) ::
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
              takesG[String](parent :: O.string -- "second" -~ req) ::
              runs[A](s => ())
            }
          }
        }: CommandDsl[Command]""")
    }

    it("compiles for command with multiple subcommands") {
      cmd("command1") {
        cmd("subcommand1") {
          takesG[String](O.string -- "first" -~ req) ::
          runs[String](s => ())
        } ::
        cmd("subcommand2") {
          takesG[String](O.string -- "second" -~ req) ::
          runs[String](s => ())
        } ::
        cmd("subcommand3") {
          takesG[String](O.string -- "third" -~ req) ::
          runs[String](s => ())
        }
      }: CommandDsl[Command]
    }

    it("compiles for command with config and subcommands") {
      case class A(parentString: String, subString: String)
      cmd("command1") {
        takesG[String](O.string --"field" -~ req) ::
        cmd("subcommand1") {
          takesG[String](O.string -- "second" -~ req) ::
          runs[A](s => ())
        }
      }: CommandDsl[Command]
    }

    it("does not compile for command with config and subcommands without parent config") {
      illTyped("""
        cmd("command1") {
          takesG[String](O.string --"field" -~ req) ::
          cmd("subcommand1") {
            takesG[String](O.string -- "second" -~ req) ::
            runs[String](s => ())
          }
        }: CommandDsl[Command]""")
    }

    it("compiles for command with config and multiple subcommands") {
      case class A(parentString: String, subString: String)
      cmd("command1") {
        takesG[String](O.string --"field" -~ req) ::
        cmd("subcommand1") {
          takesG[String](O.string -- "first" -~ req) ::
          runs[A](s => ())
        } ::
        cmd("subcommand2") {
          takesG[String](O.string -- "second" -~ req) ::
          runs[A](s => ())
        } ::
        cmd("subcommand3") {
          takesG[String](O.string -- "third" -~ req) ::
          runs[A](s => ())
        }
      }: CommandDsl[Command]
    }

    it("compiles for command with config and multiple subcommands of N depth") {
      case class A(parentString: String, subString: String)
      case class B(parent1: String, parent2: String, parent3: String, subString: String)

      cmd("command1") {
        takesG[String](O.string --"field" -~ req) ::
        cmd("subcommand1") {
          takesG[String](O.string --"subfield1" -~ req) ::
          cmd("subcommand2") {
            takesG[String](O.string -- "subfield2" -~ req) ::
            cmd("subcommand3") {
              takesG[String](O.string -- "subfield3" -~ req) ::
              runs[B](s => ())
            }
          }
        } ::
        cmd("subcommand4") {
          takesG[String](O.string -- "subfield4" -~ req) ::
          runs[A](s => ())
        } ::
        cmd("subcommand5") {
          takesG[String](O.string -- "subfield5" -~ req) ::
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
        takesG[A] {
          O.int --"intField" -~ req ::
          boolean  -~ name("boolArg")
        } ::
        cmd("subcommand1") {
          takesG[String](O.string --"subfield1" -~ req) ::
          cmd("subcommand2") {
            cmd("subcommand3") {
              takesG[String](O.string --"subfield3" -~ req) ::
              runs[B](s => ())
            }
          }
        } ::
        cmd("subcommand4") {
          takesG[String](O.string -- "subfield4" -~ req) ::
          cmd("subcommand5") {
            runs[(A, String)](s => ())
          } ::
          cmd("subcommand6") {
            takesG[Int](int -~ name("subfield6")) ::
            runs[(A, String, Int)](s => ())
          }
        } ::
        cmd("subcommand7") {
          takesG[String](O.string --"subfield7" -~ req) ::
          runs[(A, String)](s => ())
        } ::
        cmd("subcommand8") {
          takesG[C](O.string --"subfield8" -~ req) ::
          runs[D](s => ())
        }
      }: CommandDsl[Command]
    }
  }
}
