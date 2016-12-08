package pavlosgi.freecli.config

import shapeless._

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
          o.string --"a1" -~ req ::
          flag        --"a2" ::
          o.int    --"a3" -~ req ::
          sub[B]("b") {
            o.string --"b1" -~ req ::
            flag        --"b2" ::
            o.int       --"b3" ::
            sub[C]("c") {
              o.string --"c1" -~ req ::
              o.int    --"c2" -~ req
            } ::
            sub[D]("d") {
              flag -- "d1"
            }
          } ::
          o.string --"a5" -~ req ::
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
