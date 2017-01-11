package pavlosgi.freecli.argument

import shapeless._

import pavlosgi.freecli.core.all._
import pavlosgi.freecli.argument.all._
import pavlosgi.freecli.testkit.Test

class ArgumentDslTest extends Test {
  describe("Arguments Dsl tests") {

    it("allow plain arg") {
      string: ArgumentDsl[String]
    }

    it("allow adding name") {
      string -~ name("string"): ArgumentDsl[String]
    }

    it("allow adding description") {
      string -~ des("string"): ArgumentDsl[String]
    }

    it("allow adding placholder and description") {
      string -~ name("name") -~ des("string"): ArgumentDsl[String]
    }

    it("allow merging arguments") {
      string -~ name("first") ::
      string -~ name("second") -~ des("string") ::
      int ::
      string ::
      boolean: ArgumentDsl[String :: String :: Int :: String :: Boolean :: HNil]
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
      }: ArgumentDsl[(String, String, Int, String, Boolean)]
    }
  }
}
