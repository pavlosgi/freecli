package pavlosgi.freecli.option

import shapeless._
import shapeless.test.illTyped

import pavlosgi.freecli.core.all._
import pavlosgi.freecli.option.all._
import pavlosgi.freecli.testkit.Test

class OptionDslTest extends Test {
  describe("Options Dsl tests") {

    it("allow plain opt") {
      opt[String] --"opt": OptionDsl[Option[String]]
      string --"opt" -~ req: OptionDsl[String]
    }

    it("allow using default in opt") {
      string --"one" -~ or("1"): OptionDsl[String]
      string --"one" -'c' -~ or("1"): OptionDsl[String]
      string --"one" -'c' -~ or("1"): OptionDsl[String]
    }

    it("allow using default with required in opt") {
      string --"one" -~ req -~ or("1"): OptionDsl[String]
      string --"one" -'c' -~ req -~ or("2"): OptionDsl[String]
      opt[String] --"one" -'c' -~ req -~ or("1"): OptionDsl[String]
      opt[String] --"one" -'c' -~ or("1") -~ req: OptionDsl[String]
    }

    it("not allow using default or required twice") {
      illTyped("""opt[String] --"one" -~ or("1") -~ or("1"): OptionDsl[String]""")
      illTyped("""string --"one" -'c' -~ req -~ req: OptionDsl[String]""")
    }

    it("allow sub with case class") {
      case class A(value: Option[String], value2: Option[String], value3: Option[String])

      sub[A](des("description")) {
        string --"one" ::
        string --"two" ::
        opt[String] --"three"
      }: OptionDsl[A]
    }

    it("sub does not compile without subconfiguration") {
      case class A(v: String)
      illTyped("""sub[A](des("description")): OptionDsl[A]""")
    }

    it("group with help") {
      case class A(p: Option[String])
      val dsl =
        group[A] {
          help_ -'h' --"help" ::
          string -'p'
        }
    }

    it("allow merging options") {
      case class A(o: Option[String], o2: Option[Boolean], o3: Boolean)

      string --"name1" ::
      string --"name2" -~ des("name2") ::
      int -'n' ::
      sub[A](des("sub")) {
        string -'f' -~ des("f") ::
        boolean -'b' ::
        flag -'g'
      }: OptionDsl[Option[String] :: Option[String] :: Option[Int] :: A :: HNil]
    }

    it("group to single case class") {
      case class A(o: Option[String])

      group[A] {
        string --"name1"
      }
    }

    it("group to case class") {
      case class A(o: Option[String], o2: Option[String], o3: Option[Int], o4: B)
      case class B(o: Option[String], o2: Option[Boolean], o3: Boolean)

      group[A] {
        string --"name1" ::
        string --"name2" -~ des("name2") ::
        int -'n' ::
        sub[B](des("sub")) {
          string -'f' -~ des("f") ::
          boolean -'b' ::
          flag -'g'
        }
      }
    }

    it("group to tuple") {
      groupT {
        string --"name1" ::
        string --"name2" -~ des("name2") ::
        int -'n' ::
        subT(des("sub")) {
          string -'f' -~ des("f") ::
          boolean -'b' ::
          flag -'g'
        }
      }: OptionDsl[(Option[String], Option[String], Option[Int], (Option[String], Option[Boolean], Boolean))]
    }
  }
}
