package pavlosgi.freecli.core.interpreters.parser.arguments

import shapeless.{tupled => _, _}

import pavlosgi.freecli.testkit.Test
import pavlosgi.freecli.core.dsl.config._

class ArgumentsParserTest extends Test {
  describe("Arguments parser") {

    it("parse string argument") {
      val res = parseConfig(Seq("localhost"))(string)
      res.valid should === ("localhost")
    }

    it("parse int argument") {
      val res = parseConfig(Seq("1"))(int)
      res.valid should === (1)
    }

    it("parse multiple tuple") {
      val res = parseConfig(Seq("c1", "2", "c3"))(
        tupled {
          string ::
          int    ::
          string
        })

      res.valid should === (("c1", 2, "c3"))
    }

    it("parse multiple case class") {
      case class A(c1: String, c2: Int, c3: String)
      val res = parseConfig(Seq("c1", "2", "c3"))(
        gen[A] {
          string ::
          int    ::
          string
        })

      res.valid should === (A("c1", 2, "c3"))
    }
  }
}
