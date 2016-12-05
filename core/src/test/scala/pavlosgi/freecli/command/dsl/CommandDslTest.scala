//package pavlosgi.freecli.command.dsl
//
//import shapeless.test.illTyped
//
//import pavlosgi.freecli.config.dsl._
//import pavlosgi.freecli.command.api.Command
//import pavlosgi.freecli.testkit.Test
//
//class CommandDslTest extends Test {
//  describe("ConfigDsl tests") {
//
//    it("compiles for command with run") {
//      cmd("command1", "some command") {
//        runs(())
//      }: CommandDsl[Command]
//    }
//
//    it("compiles for command with config and run with same type") {
//      cmd("command1") {
//        takes(gen[String](options(req[String] --"field"))) ::
//        runs[String](s => ())
//      }: CommandDsl[Command]
//    }
//
//    it("does not compile for command with config and run with same type reversed") {
//      illTyped("""
//        cmd("command1") {
//          runs[String](s => ()) ::
//          takes(gen[String](options(req[String] --"field)")))
//        }: CommandDsl[Command]
//      """)
//    }
//
//    it("compiles for command with config and run with different type") {
//      case class A(s: String)
//
//      cmd("command1") {
//        takes(gen[A](options(req[String] --"field" -'f'))) ::
//        runs[A](s => ())
//      }: CommandDsl[Command]
//    }
//
//    it("does not compile for command with config and run with different type reversed") {
//      case class A(s: String)
//      illTyped("""
//        cmd("command1") {
//          runs[A](s => ()) ::
//          takes(gen[String](options(req[String] --"field)")))
//        }: CommandDsl[Command]
//      """)
//    }
//
//    it("compiles for command with subcommands") {
//      cmd("command1") {
//        cmd("subcommand1") {
//          takes(gen[String](options(req[String] -- "second"))) ::
//          runs[String](s => ())
//        }
//      }: CommandDsl[Command]
//    }
//
//    it("does not compile for command with config subcommands if parent has no config") {
//      case class A(parentString: String, subString: String)
//      illTyped("""
//        cmd("command1") {
//          subcommands[String] { parent =>
//            cmd("subcommand1") {
//              takes(gen[String](parent :: options(req[String] -- "second"))) ::
//              runs[A](s => ())
//            }
//          }
//        }: CommandDsl[Command]""")
//    }
//
//    it("compiles for command with multiple subcommands") {
//      cmd("command1") {
//        cmd("subcommand1") {
//          takes(gen[String](options(req[String] -- "first"))) ::
//          runs[String](s => ())
//        } ::
//        cmd("subcommand2") {
//          takes(gen[String](options(req[String] -- "second"))) ::
//          runs[String](s => ())
//        } ::
//        cmd("subcommand3") {
//          takes(gen[String](options(req[String] -- "third"))) ::
//          runs[String](s => ())
//        }
//      }: CommandDsl[Command]
//    }
//
//    it("compiles for command with config and subcommands") {
//      case class A(parentString: String, subString: String)
//      cmd("command1") {
//        takes(gen[String](options(req[String] --"field"))) ::
//        cmd("subcommand1") {
//          takes(gen[String](options(req[String] -- "second"))) ::
//          runs[A](s => ())
//        }
//      }: CommandDsl[Command]
//    }
//
//    it("does not compile for command with config and subcommands without parent config") {
//      illTyped("""
//        cmd("command1") {
//          takes(gen[String](options(req[String] --"field"))) ::
//          cmd("subcommand1") {
//            takes(gen[String](options(req[String] -- "second"))) ::
//            runs[String](s => ())
//          }
//        }: CommandDsl[Command]""")
//    }
//
//    it("compiles for command with config and multiple subcommands") {
//      case class A(parentString: String, subString: String)
//      cmd("command1") {
//        takes(gen[String](options(req[String] --"field"))) ::
//        cmd("subcommand1") {
//          takes(gen[String](options(req[String] -- "first"))) ::
//          runs[A](s => ())
//        } ::
//        cmd("subcommand2") {
//          takes(gen[String](options(req[String] -- "second"))) ::
//          runs[A](s => ())
//        } ::
//        cmd("subcommand3") {
//          takes(gen[String](options(req[String] -- "third"))) ::
//          runs[A](s => ())
//        }
//      }: CommandDsl[Command]
//    }
//
//    it("compiles for command with config and multiple subcommands of N depth") {
//      case class A(parentString: String, subString: String)
//      case class B(parent1: String, parent2: String, parent3: String, subString: String)
//
//      cmd("command1") {
//        takes(gen[String](options(req[String] --"field"))) ::
//        cmd("subcommand1") {
//          takes(gen[String](options(req[String] --"subfield1"))) ::
//          cmd("subcommand2") {
//            takes(gen[String](options(req[String] -- "subfield2"))) ::
//            cmd("subcommand3") {
//              takes(gen[String](options(req[String] -- "subfield3"))) ::
//              runs[B](s => ())
//            }
//          }
//        } ::
//        cmd("subcommand4") {
//          takes(gen[String](options(req[String] -- "subfield4"))) ::
//          runs[A](s => ())
//        } ::
//        cmd("subcommand5") {
//          takes(gen[String](options(req[String] -- "subfield5"))) ::
//          runs[A](s => ())
//        }
//      }: CommandDsl[Command]
//    }
//
//    it("compiles for command with config and multiple subcommands complex tree") {
//      case class A(b: Int, c: Boolean)
//      case class B(parent1: A, parent2: String, subString: String)
//      case class C(s: String)
//      case class D(parent1: A, sub: C)
//
//      cmd("command1") {
//        takes(gen[A] {
//          options {
//            req[Int]  --"intField"
//          } ::
//          arguments {
//            boolean   -~ name("boolArg")
//          }
//        }) ::
//        cmd("subcommand1") {
//          takes(gen[String](options(req[String] --"subfield1"))) ::
//          cmd("subcommand2") {
//            cmd("subcommand3") {
//              takes(gen[String](options(req[String] --"subfield3"))) ::
//              runs[B](s => ())
//            }
//          }
//        } ::
//        cmd("subcommand4") {
//          takes(gen[String](options(req[String] -- "subfield4"))) ::
//          cmd("subcommand5") {
//            runs[(A, String)](s => ())
//          } ::
//          cmd("subcommand6") {
//            takes(gen[Int](arguments(int -~ name("subfield6")))) ::
//            runs[(A, String, Int)](s => ())
//          }
//        } ::
//        cmd("subcommand7") {
//          takes(gen[String](options(req[String] --"subfield7"))) ::
//          runs[(A, String)](s => ())
//        } ::
//        cmd("subcommand8") {
//          takes(gen[C](options(req[String] --"subfield8"))) ::
//          runs[D](s => ())
//        }
//      }: CommandDsl[Command]
//    }
//  }
//}
