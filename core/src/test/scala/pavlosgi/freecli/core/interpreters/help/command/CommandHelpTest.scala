package pavlosgi.freecli.core.interpreters.help.command

import pavlosgi.freecli.core.dsl.command._
import pavlosgi.freecli.core.dsl.config._
import pavlosgi.freecli.testkit.Test

class CommandHelpTest extends Test {
  describe("Help") {
    it("show help") {
      case class A(b: Int, c: Boolean)
      case class B(parent1: A, parent2: String, subString: String)
      case class C(s: String)
      case class D(parent1: A, sub: C)

      val dsl =
        cmd("command1", "command1 description") {
          config[A] {
            o.int     --"intField"  ::
            o.boolean --"boolField"
          } ::
          cmd("subcommand1", "subcommand1 description") {
            config[String](o.string --"subfield1") ::
            cmd("subcommand2") {
              cmd("subcommand3") {
                config[String](o.string --"subfield3") ::
                runs[B](s => ())
              }
            }
          } ::
          cmd("subcommand4") {
            config[String](o.string -- "subfield4") ::
            cmd("subcommand5") {
              runs[(A, String)](s => ())
            } ::
            cmd("subcommand6") {
              config[Int](o.int --"subfield6") ::
              runs[(A, String, Int)](s => ())
            }
          } ::
          cmd("subcommand7") {
            config[String](o.string --"subfield7") ::
            runs[(A, String)](s => ())
          } ::
          cmd("subcommand8") {
            config[C](o.string --"subfield8") ::
            runs[D](s => ())
          }
        }

      val help = commandHelp(dsl)
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
