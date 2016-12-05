package pavlosgi.freecli.core.dsl.arguments

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

    it("gen single") {
      case class A(a1: String)
      gen[A] {
        string -~ name("first")
      }
    }

    it("gen to case class") {
      case class A(a1: String, a2: String, a3: Int, a4: String, a5: Boolean)

      gen[A] {
        string -~ name("first") ::
        string -~ name("second") -~ des("string") ::
        int ::
        string ::
        boolean
      }
    }

    it("tupled works") {
      tupled {
        string -~ name("first") ::
        string -~ name("second") -~ des("string") ::
        int ::
        string ::
        boolean
      }: ArgumentsDsl[(String, String, Int, String, Boolean)]
    }
  }
}
