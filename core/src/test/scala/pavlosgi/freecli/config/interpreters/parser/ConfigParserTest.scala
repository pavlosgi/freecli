package pavlosgi.freecli.config.interpreters.parser

import shapeless._

import pavlosgi.freecli.config.dsl._
import pavlosgi.freecli.testkit.Test

class ConfigParserTest extends Test {
  describe("Config parser") {
    it("parse complex configuration") {
      case class A(
        a1: String,
        a2: Boolean,
        a3: Int,
        a4: B,
        a5: String,
        a6: String,
        a7: Int,
        a8: Boolean)

      case class B(
        b1: String,
        b2: Boolean,
        b3: Option[Int],
        b4: C,
        b5: D)

      case class C(c1: String, c2: Int)
      case class D(d1: Boolean)

      val dsl =
        group[A] {
          req[String] --"a1" ::
          flag        --"a2" ::
          req[Int]    --"a3" ::
          sub[B]("b") {
            req[String] --"b1" ::
            flag        --"b2" ::
            o.int       --"b3" ::
            sub[C]("c") {
              req[String] --"c1" ::
              req[Int]    --"c2"
            } ::
            sub[D]("d") {
              flag -- "d1"
            }
          } ::
          req[String] --"a5" ::
          string ::
          int ::
          boolean
        }

      val args = Seq(
        "--a1",
        "a1",
        "--a2",
        "--a3",
        "3",
        "--b1",
        "b1",
        "--b2",
        "--b3",
        "3",
        "--c1",
        "c1",
        "--c2",
        "2",
        "--d1",
        "--a5",
        "a5",
        "a6",
        "7",
        "true")

      parseConfig(args)(dsl).valid should === (
        A(
          "a1",
          true,
          3,
          B("b1", true, Some(3), C("c1", 2), D(true)),
          "a5",
          "a6",
          7,
          true))
    }
  }
}
