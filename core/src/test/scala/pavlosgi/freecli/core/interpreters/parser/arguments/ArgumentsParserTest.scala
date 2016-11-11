package pavlosgi.freecli.core.interpreters.parser.arguments

import pavlosgi.freecli.core.dsl.arguments._
import pavlosgi.freecli.testkit.Test

class ArgumentsParserTest extends Test {
  describe("Arg tests") {
    it("parse string") {
      val res = parseArguments(Seq("c1"))(string)
      res.valid should === ("c1")
    }

    it("parse int") {
      val res = parseArguments(Seq("1"))(int)
      res.valid should === (1)
    }

    it("parse multiple tuple") {
      val res = parseArguments(Seq("c1", "2", "c3"))(
        arguments {
          string ::
          int    ::
          string
        })

      res.valid should === (("c1", 2, "c3"))
    }

    it("parse multiple case class") {
      case class A(c1: String, c2: Int, c3: String)
      val res = parseArguments(Seq("c1", "2", "c3"))(
        arguments[A] {
          string ::
          int    ::
          string
        })

      res.valid should === (A("c1", 2, "c3"))
    }
  }
}
