package pavlosgi.freecli.core.interpreters.help.command

import cats.syntax.show._

import pavlosgi.freecli.core.api.options.FieldName
import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.command.CommandFieldName
import pavlosgi.freecli.core.dsl.command._
import pavlosgi.freecli.core.dsl.config._
import pavlosgi.freecli.testkit.Test

class CommandHelpTest extends Test {
  describe("Help") {
    it("show help") {
      case class A(a1: Int, a2: Boolean, a3: String, a4: Int)
      case class B(b1: A, b2: String, b3: String)
      case class C(c1: String)
      case class D(d1: A, d2: C)

      val dsl =
        cmd("command1", "command1 description") {
          takes {
            gen[A] {
              options {
                req[Int]  --"a1" -~ des("a1 description")  ::
                flag --"a2"
              } ::
              arguments {
                string ::
                int
              }
            }
          } ::
          cmd("subcommand1", "subcommand1 description") {
            takes(gen[String](options(req[String] --"subfield1"))) ::
            cmd("subcommand2") {
              cmd("subcommand3") {
                takes(gen[String](options(req[String] --"subfield3"))) ::
                runs[B](s => ())
              }
            }
          } ::
          cmd("subcommand4") {
            takes(gen[String](options(req[String] -- "subfield4"))) ::
            cmd("subcommand5") {
              runs[(A, String)](s => ())
            } ::
            cmd("subcommand6") {
              takes(gen[Int](options(req[Int] --"subfield6"))) ::
              runs[(A, String, Int)](s => ())
            }
          } ::
          cmd("subcommand7") {
            takes(gen[String](options(req[String] --"subfield7"))) ::
            runs[(A, String)](s => ())
          } ::
          cmd("subcommand8") {
            takes(gen[C](options(req[String] --"subfield8"))) ::
            runs[D](s => ())
          }
        }

      val help = commandHelp(dsl)
      print(help)

      Seq(
        CommandFieldName("command1").show,
        FieldName("a1").show,
        Description("a1 description").show,
        FieldName("a2").show,
        "arg1",
        "arg2",
        CommandFieldName("subcommand1").show,
        FieldName("subfield1").show,
        CommandFieldName("subcommand2").show,
        CommandFieldName("subcommand3").show,
        FieldName("subfield3").show,
        CommandFieldName("subcommand4").show,
        FieldName("subfield4").show,
        CommandFieldName("subcommand5").show,
        CommandFieldName("subcommand6").show,
        FieldName("subfield6").show,
        CommandFieldName("subcommand7").show,
        FieldName("subfield7").show,
        CommandFieldName("subcommand8").show,
        FieldName("subfield8").show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
