package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.test.illTyped

import pavlosgi.freecli.testkit.Test

class ConfigDslTest extends Test {
  describe("ConfigDsl tests") {

    it("allow different styles") {
      req[String] --"one" -'c' -~ des("assaa")
      opt[String] --"one" -'c' -~ or("1")
      opt[String] --"one" -'c' -~ required
      opt[String] --"one" -'c' -~ or("1")
      string --"one" -'c' -~ or("1") -~ des("assaa")
      opt[String] --"one" -'c'
      string --"one" -'c'
    }

    it("allow using default in arg") {
      string --"one" -~ or("1")
      string --"one" -'c' -~ or("1")
    }

    it("allow using default with required") {
      req[String] --"one" -~ or("1")
      req[String] --"one" -'c' -~ or("2")
      opt[String] --"one" -'c' -~ required -~ or("1")
      opt[String] --"one" -'c' -~ or("1") -~ required
    }

    it("not allow using default or required twice") {
      illTyped("""opt[String] --"one" -~ or("1") -~ or("1")""")
      illTyped("""req[String] --"one" -'c' -~ required""")
    }

    it("sub compiles") {
      case class A(value: String, value2: String, value3: Option[String])
      sub[A]("description") {
        string --"one" ::
        string --"two" ::
        opt[String] --"three"
      }: ConfigDsl[A]
    }

    it("sub compiles without type") {
      sub[(String, Int)]("description") {
        string --"one" -~ or("s") ::
        int    --"two"
      }: ConfigDsl[(String, Int)]
    }

    it("sub does not compile without subconfiguration") {
      case class A(v: String)
      illTyped("""sub[A] -~ des("description"): ConfigDsl[A :: HNil]""")
    }

    it("allow converting to tuple") {
      config {
        string -- "one" -~ or("1") ::
        string -- "two" -~ or("2")
      }: ConfigDsl[(String, String)]
    }

    it("complex configuration") {
      case class A(aString: String, aFlag: Boolean, aInt: Int, b: B, aString2: String)
      case class B(bString: String, bFlag: Boolean, bOptInt: Option[Int], c: C, d: D)
      case class C(cString: String, cInt: Int)
      case class D(dFlag: Boolean)

      config[A] {
        string --"aValue" -'a' -~ des("sa") -~ or("as") ::
        flag   --"aFlag" ::
        int    --"aInt" ::
        sub[B]("b") {
          string --"bString" ::
          flag   --"bFlag"   ::
          opt[Int] --"bOptInt" ::
          sub[C]("c") {
            string --"cString" ::
            int    --"cInt" -~ or(1)
          } ::
          sub[D]("d") {
            flag --"dFlag"
          }
        } ::
        string --"aString2"
      } ::
      config[A] {
        string --"aValue" -'a' -~ des("sa") -~ or("as") ::
        flag   --"aFlag"  ::
        int    --"aInt"   ::
        sub[B]("b") {
          string --"bString" -~ des("bString") ::
          flag   --"bFlag"   ::
          opt[Int] --"bOptInt" ::
          sub[C]("c") {
            string --"cString" -~ des("cString") ::
            int    --"cInt" -~ or(1)
          } ::
          sub[D]("d") {
            flag -- "dFlag"
          }
        } ::
        string --"aString2"
      }
    }
  }
}
