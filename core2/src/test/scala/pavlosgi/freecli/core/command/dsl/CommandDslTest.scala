package pavlosgi.freecli.core.command.dsl

//import shapeless.test.illTyped

import shapeless._

import pavlosgi.freecli.core.command.api.Command

//import pavlosgi.freecli.core.command.api.CommandFieldName
//import pavlosgi.freecli.core.command.dsl.CommandFieldBuilder.CanProduceCommandField
//import pavlosgi.freecli.core.command.dsl.PartialCommandDsl.CommandRun
import pavlosgi.freecli.core.dsl.config._
import pavlosgi.freecli.testkit.Test

class CommandDslTest extends Test {
  describe("ConfigDsl tests") {

    it("allow using default in arg") {
      cmd("command1") {
        takes(string --"ss") ::
        runs[String] { s =>
          ()
        }
      }
// found   : CommandFieldBuilder.CanProduceCommandField[
//   shapeless.::[pavlosgi.freecli.core.command.api.CommandFieldName,
//     shapeless.::[PartialCommandDsl.CommandRun[Unit],shapeless.HNil]]]{type DOut =
// shapeless.::[PartialCommandDsl.CommandRun[Unit],shapeless.HNil]}
//[error]  required: CommandFieldBuilder.CanProduceCommandField.Aux[
//        shapeless.::[pavlosgi.freecli.core.command.api.CommandFieldName,
//          shapeless.::[PartialCommandDsl[
//            shapeless.::[PartialCommandDsl.CommandRun[Unit],shapeless.HNil],Unit],shapeless.HNil]],?]
//[error]     (which expands to)  CommandFieldBuilder.CanProduceCommandField[
//        shapeless.::[pavlosgi.freecli.core.command.api.CommandFieldName,
//          shapeless.::[PartialCommandDsl[
//            shapeless.::[PartialCommandDsl.CommandRun[Unit],shapeless.HNil],Unit],shapeless.HNil]]]{type DOut = ?}

      case class A(s: String)
      case class B(s: String, s2: String)

//      implicitly[CanProduceCommandField[CommandFieldName :: CommandRun[Unit] :: HNil]]
//
//      implicitly[pavlosgi.freecli.core.command.dsl.PartialCommandDsl.CanProduceCommandDsl[shapeless.::[pavlosgi.freecli.core.command.api.CommandFieldName,shapeless.::[pavlosgi.freecli.core.command.dsl.PartialCommandDsl[shapeless.::[pavlosgi.freecli.core.command.dsl.PartialCommandDsl.CommandRun[Unit],shapeless.HNil],Unit],shapeless.HNil]],Unit]]

      cmd("command1") {
        takes(string --"ss") ::
        subcommands[String] { parent =>
          cmd("command2") {
            takes(string --"ss") ::
            runs[A](_ => ())
          }: CommandDsl[Command]
//          cmd("command2") {
//            runs(())
//          }
        }
      }
//      cmd("my-command") {
//        dsl.run(())
//      }

//      cmd("my-command") {
//        takes(string --"one") and
//        runs(s => ())
//      }
//        cmd("my-command1") ->
//          (string --"one") -->
//          (c => c.length) |
//
//        cmd("my-command2") ->
//          (string --"one") -->
//          (c => c.length)
//        cmd("my-command1", Some("command1")) {
//          string -- "one" ::
//          cmd("my-command2") {
//            string -- "two" ::
//            dsl.run((s1, s2) => {
//              ()
//            })
//          }
//        }

//        case class A(parent: String, me: String)

//        cmd("my-command1", Some("command1")) {
//          PartialCommandConfigurationDsl.fromPartialConfigDsl(string -- "one") ~
//          PartialCommandConfigurationDsl.fromCommandDsl(cmd("my-command1", Some("command1")) {
//            dsl.run[String](_ => ())
//          })
//        }


//        cmd("my-command1") -? "command1" ->> (string --"one") --> (c => c.length) |
//        cmd("my-command2") -? "command2" ->> (string --"one") -*> { c =>
//          cmd("my-command3") -? "command3" ->> (string --"one") --> (c => c.length) |
//          cmd("my-command4") -? "command4" --/> (())
//        } |
//        cmd("my-command5") -? "command5" ->> (string --"one") -*> { c =>
//          cmd("my-command6") -? "command6" ->> (string --"one") -*> { c =>
//            cmd("my-command7") -? "command7" ->> (string -- "one") -*> { c =>
//              cmd("my-command8") -? "command8" --/> (())
//            }
//          }
//        }
//        cmd("my-command2") -? "saas" --> (())
//      string --"one" -| "1"
//      string --"one" -'c' -| "1"
    }

//    it("not allow using default in opt") {
//      illTyped("""optString --"one" -| "1"""")
//      illTyped("""optString --"one" -'c' -| "1"""")
//    }
//
//    it("allow using default in flag") {
//      flag --"one" -| true
//      flag --"one" -'c' -| false
//    }
//
//    it("sub compiles") {
//      case class A(value: String)
//        sub[A] -?"description" -& {
//          string --"one"
//        } : ConfigDsl[A :: HNil]
//
//    }
//
//    it("sub does not compile without subconfiguration") {
//      case class A(v: String)
//      illTyped("""sub[A] -?"description": ConfigDsl[A :: HNil]""")
//    }
//
//    it("not allow using field name in sub") {
//      case class A(value: String)
//      illTyped(
//        """sub[A] --"name" -?"description" -& {
//             string --"one"
//          } : ConfigDsl[A :: HNil]""")
//    }
//
//    it("not allow using field abbreviation in sub") {
//      case class A(value: String)
//      illTyped(
//        """sub[A] -'n' -?"description" -& {
//             string --"one"
//          } : ConfigDsl[A :: HNil]""")
//    }
//
//    it("not allow using default in sub") {
//      case class A(value: String)
//      illTyped(
//        """sub[A] -| A("1") -?"description" -& {
//             string --"one"
//          } : ConfigDsl[A :: HNil]""")
//    }
//
//    it("complex configuration") {
//      case class A(aString: String, aFlag: Boolean, aInt: Int, b: B, aString2: String)
//      case class B(bString: String, bFlag: Boolean, bOptInt: Option[Int], c: C, d: D)
//      case class C(cString: String, cInt: Int)
//      case class D(dFlag: Boolean)
//
//      config[A] {
//        string --"aValue" ::
//        flag   --"aFlag"  ::
//        int    --"aInt"   ::
//        sub[B] -? "b" -& {
//          string --"bString" ::
//          flag   --"bFlag"   ::
//          optInt --"bOptInt" ::
//          sub[C] -? "c" -& {
//            string --"cString" ::
//            int    --"cInt"
//          } ::
//          sub[D] -? "d" -& {
//            flag -- "dFlag"
//          }
//        } ::
//        string --"aString2"
//      }
//    }
  }
}
