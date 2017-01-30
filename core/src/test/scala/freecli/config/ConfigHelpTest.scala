package freecli
package config

import cats.syntax.show._

import argument.api._
import config.all._
import core.all._
import core.api.Description
import option.api._
import testkit.Test

class ConfigHelpTest extends Test {
  describe("Config help") {
    it("show config help") {
      case class A(
        a1: Option[String],
        a2: Option[Int],
        a3: B,
        a4: Boolean,
        a5: String,
        a6: String,
        a7: String)

      case class B(b1: Option[String], b2: Option[Int], b3: Boolean, b4: String, b5: C)
      case class C(c1: Option[String], c2: Option[Int])

      val dsl =
        group[A] {
          O.string --"a1" -'a' -~ des("a1_description") ::
          O.int    --"a2" -~ des("a2_description")      ::
          sub[B](des("a3 options")) {
            O.string --"b1" -'b' -~ des("b1_description") ::
            O.int    --"b2" -'c' -~ des("b2_description") ::
            flag     -'d' ::
            O.string -'e' -~ or("default") -~ des("e option") ::
            sub[C](des("b5 options")) {
              O.string --"c1" ::
              O.int -'c'
            }
          } ::
          flag --"a4" ::
          string -~ name("a5") -~ des("a5_description") ::
          string -~ des("a6_description") ::
          string -~ des("a7_description")
        }

      val help = configHelp(dsl)
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
        OptionFieldName("a4").show,
        ArgumentFieldName("a5").show,
        ArgumentFieldName("arg2").show,
        ArgumentFieldName("arg3").show,
        Description("a5_description").show,
        Description("a6_description").show,
        Description("a7_description").show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
