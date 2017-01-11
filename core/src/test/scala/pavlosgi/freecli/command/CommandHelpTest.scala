package pavlosgi.freecli.command

import cats.syntax.show._

import pavlosgi.freecli.core.all._
import pavlosgi.freecli.command.api.CommandFieldName
import pavlosgi.freecli.command.all._
import pavlosgi.freecli.config.all._
import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.option.api.OptionFieldName
import pavlosgi.freecli.testkit.Test

class CommandHelpTest extends Test {
  describe("Help") {
    it("show help") {
      case class A(a1: Int, a2: Boolean, a3: String, a4: Int)
      case class B(b1: A, b2: String, b3: String)
      case class C(c1: String)
      case class D(d1: A, d2: C)

      val dsl =
        cmd("command1", des("command1 description")) {
          takesG[A] {
            O.int  --"a1" -~ req -~ des("a1 description")  ::
            flag --"a2" ::
            string ::
            int
          } ::
          cmd("subcommand1", des("subcommand1 description")) {
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
              takesG[Int](O.int --"subfield6" -~ req) ::
              runs[(A, String, Int)](s => ())
            }
          } ::
          cmd("subcommand7") {
            takesT(O.string --"subfield7" -~ req :: int) ::
            runs[(A, (String, Int))](s => ())
          } ::
          cmd("subcommand8") {
            takesG[C](O.string --"subfield8" -~ req) ::
            runs[D](s => ())
          }
        }

      val help = commandHelp(dsl)
      print(help)

      Seq(
        CommandFieldName("command1").show,
        OptionFieldName("a1").show,
        Description("a1 description").show,
        OptionFieldName("a2").show,
        "arg1",
        "arg2",
        CommandFieldName("subcommand1").show,
        OptionFieldName("subfield1").show,
        CommandFieldName("subcommand2").show,
        CommandFieldName("subcommand3").show,
        OptionFieldName("subfield3").show,
        CommandFieldName("subcommand4").show,
        OptionFieldName("subfield4").show,
        CommandFieldName("subcommand5").show,
        CommandFieldName("subcommand6").show,
        OptionFieldName("subfield6").show,
        CommandFieldName("subcommand7").show,
        OptionFieldName("subfield7").show,
        CommandFieldName("subcommand8").show,
        OptionFieldName("subfield8").show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
