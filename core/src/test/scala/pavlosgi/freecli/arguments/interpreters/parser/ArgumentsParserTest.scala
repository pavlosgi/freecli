package pavlosgi.freecli.arguments.interpreters.parser

import shapeless.{tupled => _}

import pavlosgi.freecli.config.dsl._
import pavlosgi.freecli.testkit.Test

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
        groupT {
          string ::
          int    ::
          string
        })

      res.valid should === (("c1", 2, "c3"))
    }

    it("parse multiple case class") {
      case class A(c1: String, c2: Int, c3: String)
      val res = parseConfig(Seq("c1", "2", "c3"))(
        group[A] {
          string ::
          int    ::
          string
        })

      res.valid should === (A("c1", 2, "c3"))
    }
  }
}
