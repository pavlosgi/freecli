package pavlosgi.freecli.core.interpreters.help.arguments

import cats.syntax.show._

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.arguments._
import pavlosgi.freecli.core.dsl.arguments._
import pavlosgi.freecli.testkit.Test

class ArgumentsHelpTest extends Test {
  describe("Arguments help") {
    it("show arguments help") {
      case class A(a1: Int, a2: String, a3: String)

      val dsl =
        gen[A] {
          int -~ name("a1") ::
          string -~ name("a2") -~ des("a2_description") ::
          string -~ des("a3_description")
        }

      val help = argumentsHelp(dsl)
      print(help)

      Seq(
        ArgumentName("a1").show,
        ArgumentName("a2").show,
        Description("a2_description").show,
        ArgumentName("a3").show).foreach { keyword =>
          withClue(s"$keyword not found in $help") {
            help.contains(keyword) should be (true)
          }
      }
    }
  }
}
