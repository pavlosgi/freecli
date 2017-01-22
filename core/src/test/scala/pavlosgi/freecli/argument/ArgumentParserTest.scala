package pavlosgi.freecli.argument

import pavlosgi.freecli.core.all._
import pavlosgi.freecli.argument.all._
import pavlosgi.freecli.testkit.Test
import pavlosgi.freecli.Helpers._

class ArgumentParserTest extends Test {
  describe("Arguments parser") {

    it("parse string argument") {
      val res = parseArgument(string).run(Seq("localhost"))
      res.success should === ("localhost")
    }

    it("parse int argument") {
      val res = parseArgument(int).run(Seq("1"))
      res.success should === (1)
    }

    it("parse multiple tuple") {
      val res = parseArgument(
        groupT {
          string ::
          int    ::
          string
        }).run(Seq("c1", "2", "c3"))

      res.success should === (("c1", 2, "c3"))
    }

    it("parse multiple case class") {
      case class A(c1: String, c2: Int, c3: String)
      val res = parseArgument(
        group[A] {
          string ::
          int    ::
          string
        }).run(Seq("c1", "2", "c3"))

      res.success should === (A("c1", 2, "c3"))
    }
  }
}
