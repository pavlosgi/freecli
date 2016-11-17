package pavlosgi.freecli.core.interpreters.help.config

import cats.syntax.show._

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.testkit.Test
import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config._

class ConfigHelpTest extends Test {
  describe("Help") {
    it("show help") {
      case class A(a1: String, a2: Int, a3: B, a4: Boolean, a5: String, a6: String)
      case class B(b1: String, b2: Int, b3: Boolean, b4: String)

      val dsl =
        config[A] {
          o.string --"a1" -'a' -~ des("a1_description") ::
          o.int    --"a2" -~ des("a2_description")      ::
          sub[B]("a3") {
            o.string --"b1" -'b' -~ des("b1_description") ::
            o.int    --"b2" -'c' -~ des("b2_description") ::
            flag     -'d' ::
            o.string -'e' -~ or("default")
          } ::
          flag --"a4" ::
          string -~ name("a5") -~ des("a5_description") ::
          string -~ des("a6_description")
        }

      val help = configHelp(dsl)
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
        FieldName("a4").show,
        Placeholder("a5").show,
        Description("a5_description").show,
        Description("a6_description").show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
