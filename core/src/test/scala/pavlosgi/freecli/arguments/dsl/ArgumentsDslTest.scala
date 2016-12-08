package pavlosgi.freecli.arguments.dsl

import shapeless._

import pavlosgi.freecli.testkit.Test

class ArgumentsDslTest extends Test {
  describe("ArgumentsDsl tests") {

    it("allow plain arg") {
      string: ArgumentsDsl[String]
    }

    it("allow adding name") {
      string -~ name("string"): ArgumentsDsl[String]
    }

    it("allow adding description") {
      string -~ des("string"): ArgumentsDsl[String]
    }

    it("allow adding placholder and description") {
      string -~ name("name") -~ des("string"): ArgumentsDsl[String]
    }

    it("allow merging arguments") {
      string -~ name("first") ::
      string -~ name("second") -~ des("string") ::
      int ::
      string ::
      boolean: ArgumentsDsl[String :: String :: Int :: String :: Boolean :: HNil]
    }

    it("group single") {
      case class A(a1: String)
      group[A] {
        string -~ name("first")
      }
    }

    it("group to case class") {
      case class A(a1: String, a2: String, a3: Int, a4: String, a5: Boolean)

      group[A] {
        string -~ name("first") ::
        string -~ name("second") -~ des("string") ::
        int ::
        string ::
        boolean
      }
    }

    it("group to tuple") {
      groupT {
        string -~ name("first") ::
        string -~ name("second") -~ des("string") ::
        int ::
        string ::
        boolean
      }: ArgumentsDsl[(String, String, Int, String, Boolean)]
    }
  }
}
