package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.test.illTyped

import pavlosgi.freecli.testkit.Test

class ConfigDslTest extends Test {
  describe("ConfigDsl tests") {

    describe("Argument tests") {
      it("allow plain arg") {
        string: ConfigDsl[String]
      }

      it("allow adding placeholder") {
        string -~ name("string"): ConfigDsl[String]
      }

      it("allow adding description") {
        string -~ des("string"): ConfigDsl[String]
      }

      it("allow adding placholder and description") {
        string -~ name("name") -~ des("string"): ConfigDsl[String]
      }
    }

    describe("Option tests") {
      it("allow plain opt") {
        opt[String] --"opt": ConfigDsl[Option[String]]
        req[String] --"opt": ConfigDsl[String]
      }

      it("allow using default in opt") {
        o.string --"one" -~ or("1"): ConfigDsl[String]
        o.string --"one" -'c' -~ or("1"): ConfigDsl[String]
      }

      it("allow using default with required in opt") {
        req[String] --"one" -~ or("1"): ConfigDsl[String]
        req[String] --"one" -'c' -~ or("2"): ConfigDsl[String]
        opt[String] --"one" -'c' -~ required -~ or("1"): ConfigDsl[String]
        opt[String] --"one" -'c' -~ or("1") -~ required: ConfigDsl[String]
      }

      it("not allow using default or required twice") {
        illTyped("""opt[String] --"one" -~ or("1") -~ or("1"): ConfigDsl[String]""")
        illTyped("""req[String] --"one" -'c' -~ required: ConfigDsl[String]""")
      }

      it("allow sub with case class") {
        case class A(value: String, value2: String, value3: Option[String])
        sub[A]("description") {
          o.string --"one" ::
          o.string --"two" ::
          opt[String] --"three"
        }: ConfigDsl[A]
      }

      it("sub compiles without type and produces tuple") {
        sub[(String, Int)]("description") {
          o.string --"one" -~ or("s") ::
          o.int    --"two"
        }: ConfigDsl[(String, Int)]
      }

      it("sub does not compile without subconfiguration") {
        case class A(v: String)
        illTyped("""sub[A]("description"): ConfigDsl[A]""")
      }
    }

    it("allow mixing to tuple") {
      config {
        optString --"optName" ::
        string -~ name("name") -~ des("string") ::
        int ::
        string -~ name("name2") ::
        boolean
      }: ConfigDsl[(Option[String], String, Int, String, Boolean)]
    }

    it("allow mixing to case class") {
      case class A(s: String, b: Int, c: Option[Int], d: String, e: Boolean)
      config[A] {
        string -~ name("name") -~ des("string") ::
        int ::
        optInt --"opt1" ::
        string -~ name("name2") ::
        boolean
      }: ConfigDsl[A]
    }


    it("config without type produces tuple") {
      config {
        o.string -- "one" -~ or("1") ::
        o.string -- "two" -~ or("2")
      }: ConfigDsl[(String, String)]
    }

    it("complex configuration") {
      case class A(a1: String,
        a2: Boolean,
        a3: Int,
        a4: B,
        a5: String,
        a6: String,
        a7: Boolean,
        a8: Int)

      case class B(b1: String, b2: Boolean, b3: Option[Int], b4: C, b5: D)
      case class C(c1: String, c2: Int)
      case class D(d1: Boolean)

      config[A] {
        o.string --"a1" -'a' -~ des("a1") -~ or("a1") ::
        flag   --"a2" ::
        o.int    --"a3" ::
        sub[B]("a4") {
          o.string --"b1" ::
          flag   --"b2"   ::
          opt[Int] --"b3" ::
          sub[C]("b4") {
            o.string --"c1" ::
            o.int    --"c2" -~ or(2)
          } ::
          sub[D]("c3") {
            flag --"d1"
          }
        } ::
        o.string --"a5" ::
        string ::
        boolean ::
        int
      }
    }
  }
}
