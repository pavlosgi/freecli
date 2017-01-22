package pavlosgi.freecli.option

import cats.syntax.show._

import pavlosgi.freecli.option.api._
import pavlosgi.freecli.core.all._
import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.option.all._
import pavlosgi.freecli.testkit.Test

class OptionHelpTest extends Test {
  describe("Options help") {
    it("show options help") {
      case class A(a1: Option[String], a2: Option[Int], a3: B, a4: Boolean)
      case class B(b1: Option[String], b2: Option[Int], b3: Boolean, b4: String, b5: C)
      case class C(c1: Option[String], c2: Option[Int])

      val dsl =
        group[A] {
          string --"a1" -'a' -~ des("a1_description") ::
          int    --"a2" -~ des("a2_description")      ::
          sub[B](des("a3 options")) {
            string --"b1" -'b' -~ des("b1_description") ::
            int    --"b2" -'c' -~ des("b2_description") ::
            flag     -'d' ::
            string -'e' -~ or("default") -~ des("e option") ::
            sub[C](des("b5 options")) {
              string --"c1" ::
              int -'c'
            }
          } ::
          version --"version" -~ ops.value("v1") ::
          flag --"a4"
        }

      val help = optionHelp(dsl)
      print(help)

      Seq(
        OptionFieldName("a1").show,
        OptionFieldAbbreviation('a').show,
        Description("a1_description").show,
        OptionFieldName("a2").show,
        Description("a2_description").show,
        OptionFieldName("b1").show,
        OptionFieldAbbreviation('b').show,
        Description("b1_description").show,
        OptionFieldName("b2").show,
        OptionFieldAbbreviation('c').show,
        Description("b2_description").show,
        OptionFieldAbbreviation('d').show,
        OptionFieldAbbreviation('e').show,
        Description("b5 options").show,
        OptionFieldName("c1").show,
        OptionFieldAbbreviation('c').show,
        OptionFieldName("version").show,
        OptionFieldName("a4").show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
