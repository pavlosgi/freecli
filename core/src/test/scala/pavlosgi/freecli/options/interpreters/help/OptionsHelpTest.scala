package pavlosgi.freecli.options.interpreters.help

import cats.syntax.show._

import pavlosgi.freecli.core.Description
import pavlosgi.freecli.options.api._
import pavlosgi.freecli.options.dsl._
import pavlosgi.freecli.testkit.Test

class OptionsHelpTest extends Test {
  describe("Options help") {
    it("show options help") {
      case class A(a1: Option[String], a2: Option[Int], a3: B, a4: Boolean)
      case class B(b1: Option[String], b2: Option[Int], b3: Boolean, b4: String, b5: C)
      case class C(c1: Option[String], c2: Option[Int])

      val dsl =
        group[A] {
          string --"a1" -'a' -~ des("a1_description") ::
          int    --"a2" -~ des("a2_description")      ::
          sub[B]("a3 options") {
            string --"b1" -'b' -~ des("b1_description") ::
            int    --"b2" -'c' -~ des("b2_description") ::
            flag     -'d' ::
            string -'e' -~ or("default") -~ des("e option") ::
            sub[C]("b5 options") {
              string --"c1" ::
              int -'c'
            }
          } ::
          flag --"a4"
        }

      val help = optionsHelp(dsl)
      print(help)

      Seq(
        FieldName("a1").show,
        FieldAbbreviation('a').show,
        Description("a1_description").show,
        FieldName("a2").show,
        Description("a2_description").show,
        FieldName("b1").show,
        FieldAbbreviation('b').show,
        Description("b1_description").show,
        FieldName("b2").show,
        FieldAbbreviation('c').show,
        Description("b2_description").show,
        FieldAbbreviation('d').show,
        FieldAbbreviation('e').show,
        FieldName("a4").show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
