package pavlosgi.freecli.core.interpreters.help.arguments

import pavlosgi.freecli.core.dsl.arguments._
import pavlosgi.freecli.testkit.Test

class ArgumentsHelpTest extends Test {
  describe("Help") {
    it("show help") {
      case class Args(c1: String, c2: Int, c3: Boolean, c4: String, c5: Int)

      val dsl =
        arguments[Args] {
          string  ::
          int     ::
          boolean ::
          string  ::
          int
        }

      val help = argumentsHelp(dsl)
      print(help)

//      Seq(
//        FieldName("host").show,
//        FieldAbbreviation('h').show,
//        Description("Server host").show,
//        FieldName("port").show,
//        Description("PostgreSQL configuration").show,
//        FieldName("pg-host").show,
//        FieldAbbreviation('g').show,
//        Description("PostgreSQL host").show,
//        FieldName("pg-port").show,
//        FieldAbbreviation('p').show,
//        Description("PostgreSQL port").show,
//        FieldAbbreviation('d').show).foreach { keyword =>
//          withClue(s"$keyword not found in $help") {
//            help.contains(keyword) should be (true)
//          }
//      }
    }
  }
}
