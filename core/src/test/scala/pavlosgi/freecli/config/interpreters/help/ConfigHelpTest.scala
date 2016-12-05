package pavlosgi.freecli.config.interpreters.help

import cats.syntax.show._

import pavlosgi.freecli.arguments.api._
import pavlosgi.freecli.core.Description
import pavlosgi.freecli.config.dsl._
import pavlosgi.freecli.options.api._
import pavlosgi.freecli.testkit.Test

class ConfigHelpTest extends Test {
  describe("Config help") {
    it("show config help") {
      case class A(a1: Option[String], a2: Option[Int], a3: B, a4: Boolean, a5: String, a6: String)
      case class B(b1: Option[String], b2: Option[Int], b3: Boolean, b4: String, b5: C)
      case class C(c1: Option[String], c2: Option[Int])

      val dsl =
        gen[A] {
          options {
            o.string --"a1" -'a' -~ des("a1_description") ::
            o.int    --"a2" -~ des("a2_description")      ::
            sub("a3 options") {
              gen[B] {
                o.string --"b1" -'b' -~ des("b1_description") ::
                o.int    --"b2" -'c' -~ des("b2_description") ::
                flag     -'d' ::
                o.string -'e' -~ or("default") -~ des("e option") ::
                sub("b5 options") {
                  gen[C] {
                    o.string --"c1" ::
                    o.int -'c'
                  }
                }
              }
            } ::
            flag --"a4"
          } ::
          arguments {
            string -~ name("a5") -~ des("a5_description") ::
            string -~ des("a6_description")
          }
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
        ArgumentName("a5").show,
        Description("a5_description").show,
        Description("a6_description").show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
