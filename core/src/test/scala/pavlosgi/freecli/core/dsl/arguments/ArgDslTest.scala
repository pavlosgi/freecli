package pavlosgi.freecli.core.dsl.arguments

import pavlosgi.freecli.testkit.Test

class ArgDslTest extends Test {
  describe("ArgDsl tests") {
    it("allow plain") {
      string: ArgDsl[String]
    }

    it("allow adding placeholder") {
      string -~ name("string"): ArgDsl[String]
    }

    it("allow adding description") {
      string -~ des("string"): ArgDsl[String]
    }

    it("allow adding placholder and description") {
      string -~ name("name") -~ des("string"): ArgDsl[String]
    }

    it("allow mixing to tuple") {
      arguments {
        string -~ name("name") -~ des("string") ::
        int ::
        string -~ name("name2") ::
        boolean
      }: ArgDsl[(String, Int, String, Boolean)]
    }

    it("allow mixing to case class") {
      case class A(s: String, b: Int, c: String, d: Boolean)
      arguments[A] {
        string -~ name("name") -~ des("string") ::
        int ::
        string -~ name("name2") ::
        boolean
      }: ArgDsl[A]
    }
  }
}
