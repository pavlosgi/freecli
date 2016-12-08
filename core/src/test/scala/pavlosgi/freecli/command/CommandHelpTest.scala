package pavlosgi.freecli.command

import cats.syntax.show._

import pavlosgi.freecli.command.api.CommandFieldName
import pavlosgi.freecli.config._
import pavlosgi.freecli.core.Description
import pavlosgi.freecli.option.api.FieldName
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
            group[A] {
              o.int  --"a1" -~ req -~ des("a1 description")  ::
              flag --"a2" ::
              string ::
              int
            }
          } ::
          cmd("subcommand1", "subcommand1 description") {
            takes(group[String](o.string --"subfield1" -~ req)) ::
            cmd("subcommand2") {
              cmd("subcommand3") {
                takes(group[String](o.string --"subfield3" -~ req)) ::
                runs[B](s => ())
              }
            }
          } ::
          cmd("subcommand4") {
            takes(group[String](o.string -- "subfield4" -~ req)) ::
            cmd("subcommand5") {
              runs[(A, String)](s => ())
            } ::
            cmd("subcommand6") {
              takes(group[Int](o.int --"subfield6" -~ req)) ::
              runs[(A, String, Int)](s => ())
            }
          } ::
          cmd("subcommand7") {
            takes(groupT(o.string --"subfield7" -~ req :: int)) ::
            runs[(A, (String, Int))](s => ())
          } ::
          cmd("subcommand8") {
            takes(group[C](o.string --"subfield8" -~ req)) ::
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
