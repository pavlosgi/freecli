package freecli
package argument

import all._
import core.all._
import Helpers._
import testkit.Test

class ArgumentParserTest extends Test {
  describe("Arguments parser") {

    it("parse string argument") {
      val (_, res) = parseArgument(string).run(Seq("localhost"))
      res.success should === ("localhost")
    }

    it("parse int argument") {
      val (_, res) = parseArgument(int).run(Seq("1"))
      res.success should === (1)
    }

    it("parse multiple tuple") {
      val (_, res) = parseArgument(
        groupT {
          string ::
          int    ::
          string
        }).run(Seq("c1", "2", "c3"))

      res.success should === (("c1", 2, "c3"))
    }

    it("parse multiple case class") {
      case class A(c1: String, c2: Int, c3: String)
      val (_, res) = parseArgument(
        group[A] {
          string ::
          int    ::
          string
        }).run(Seq("c1", "2", "c3"))

      res.success should === (A("c1", 2, "c3"))
    }
  }
}
