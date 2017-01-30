package freecli
package argument

import cats.syntax.show._

import api._
import all._
import core.all._
import core.api.Description
import testkit.Test

class ArgumentHelpTest extends Test {
  describe("Arguments help") {
    it("show arguments help") {
      case class A(a1: Int, a2: String, a3: String)

      val dsl =
        group[A] {
          int -~ name("a1") ::
          string -~ name("a2") -~ des("a2_description") ::
          string -~ des("a3_description")
        }

      val help = argumentHelp(dsl)
      print(help)

      Seq(
        ArgumentFieldName("a1").show,
        ArgumentFieldName("a2").show,
        Description("a2_description").show,
        ArgumentFieldName("a3").show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
