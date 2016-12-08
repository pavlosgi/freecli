package pavlosgi.freecli.config.dsl

import shapeless._

import pavlosgi.freecli.testkit.Test

class ConfigDslTest extends Test {
  describe("ConfigDsl tests") {

    it("allow mixing to tuple") {
      case class A(s: String, s2: Int, s3: String, s4: Boolean)

      groupT {
        o.string --"optName" ::
        o.int    --"optInt" ::
        group[A] {
          string -~ name("name") -~ des("string") ::
          int ::
          string -~ name("name2") ::
          boolean
        }
      }: ConfigDsl[(Option[String], Option[Int], A)]
    }

    it("allow mixing to case class") {
      case class A(a1: Option[Int], aa: Option[String], a2: String, a3: Int, a4: String, a5: Boolean)
      group[A] {
        o.int --"opt1" ::
        o.string --"opt2" ::
        string -~ name("name") -~ des("string") ::
        int ::
        string -~ name("name2") ::
        boolean
      }: ConfigDsl[A]
    }

    it("config from options") {
      case class A(a1: Option[Int], aa: Option[String], a3: Boolean)
      group[A] {
        o.int --"opt1" ::
        o.string --"opt2" ::
        flag --"opt3"
      }: ConfigDsl[A]
    }

    it("config from single option") {
      o.int --"opt1": ConfigDsl[Option[Int]]
    }

    it("config from arguments") {
      case class A(a1: Int, aa: String, a3: Boolean)
      group[A] {
        int -~ name("arg1") ::
        string -~ name("arg2") ::
        boolean -~ name("arg3")
      }: ConfigDsl[A]
    }

    it("config from single argument") {
      group[Int] {
        int -~ name("arg1")
      }: ConfigDsl[Int]
    }

    it("complex configuration") {
      case class A(
        a1: String,
        a2: Boolean,
        a3: Option[Int],
        a4: B,
        a5: Option[String],
        a6: String,
        a7: Boolean,
        a8: Int)

      case class B(b1: Option[String], b2: Boolean, b3: Option[Int], b4: C, b5: D)
      case class C(c1: Option[String], c2: Int)
      case class D(d1: Boolean)

      group[A] {
        o.string --"a1" -'a' -~ des("a1") -~ or("a1") ::
        flag     --"a2" ::
        o.int    --"a3" ::
        sub[B]("a4") {
          o.string --"b1" ::
          flag     --"b2" ::
          opt[Int] --"b3" ::
          sub[C]("b4") {
            o.string --"c1" ::
            o.int    --"c2" -~ or(2)
          } ::
          sub[D]("c3") {
            flag -- "d1"
          }
        } ::
        o.string --"a5" -~ des("a5") ::
        string ::
        boolean ::
        int
      }
    }
  }
}
