package pavlosgi.freecli.core.dsl.options

import shapeless._

import shapeless.test.illTyped

import pavlosgi.freecli.testkit.Test

class OptionsDslTest extends Test {
  describe("OptionsDsl tests") {

    it("allow plain opt") {
      opt[String] --"opt": OptionsDsl[Option[String]]
      req[String] --"opt": OptionsDsl[String]
    }

    it("allow using default in opt") {
      string --"one" -~ or("1"): OptionsDsl[String]
      string --"one" -'c' -~ or("1"): OptionsDsl[String]
      string --"one" -'c' -~ or("1"): OptionsDsl[String]
    }

    it("allow using default with required in opt") {
      req[String] --"one" -~ or("1"): OptionsDsl[String]
      req[String] --"one" -'c' -~ or("2"): OptionsDsl[String]
      opt[String] --"one" -'c' -~ required -~ or("1"): OptionsDsl[String]
      opt[String] --"one" -'c' -~ or("1") -~ required: OptionsDsl[String]
    }

    it("not allow using default or required twice") {
      illTyped("""opt[String] --"one" -~ or("1") -~ or("1"): OptionsDsl[String]""")
      illTyped("""req[String] --"one" -'c' -~ required: OptionsDsl[String]""")
    }

    it("allow sub with case class") {
      case class A(value: Option[String], value2: Option[String], value3: Option[String])

      sub("description") {
        gen[A] {
          string --"one" ::
          string --"two" ::
          opt[String] --"three"
        }
      }: OptionsDsl[A]
    }

    it("sub does not compile without subconfiguration") {
      case class A(v: String)
      illTyped("""sub[A]("description"): OptionsDsl[A]""")
    }

    it("allow merging options") {
      case class A(o: Option[String], o2: Option[Boolean], o3: Boolean)

      string --"name1" ::
      string --"name2" -~ des("name2") ::
      int -'n' ::
      sub("sub") {
        gen[A] {
          string -'f' -~ des("f") ::
          boolean -'b' ::
          flag -'g'
        }
      }: OptionsDsl[Option[String] :: Option[String] :: Option[Int] :: A :: HNil]
    }

    it("gen to single case class") {
      case class A(o: Option[String])

      gen[A] {
        string --"name1"
      }
    }

    it("gen to case class") {
      case class A(o: Option[String], o2: Option[String], o3: Option[Int], o4: B)
      case class B(o: Option[String], o2: Option[Boolean], o3: Boolean)

      gen[A] {
        string --"name1" ::
        string --"name2" -~ des("name2") ::
        int -'n' ::
        sub("sub") {
          gen[B] {
            string -'f' -~ des("f") ::
            boolean -'b' ::
            flag -'g'
          }
        }
      }
    }

    it("tupled") {
      tupled {
        string --"name1" ::
        string --"name2" -~ des("name2") ::
        int -'n' ::
        sub("sub") {
          tupled {
            string -'f' -~ des("f") ::
            boolean -'b' ::
            flag -'g'
          }
        }
      }: OptionsDsl[(Option[String], Option[String], Option[Int], (Option[String], Option[Boolean], Boolean))]
    }
  }
}
