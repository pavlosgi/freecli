package freecli
package config

import core.all._
import config.all._
import testkit.Test

class ConfigDslTest extends Test {
  describe("ConfigDsl tests") {

    it("allow mixing to tuple") {
      case class Config(s: String, s2: Int, s3: String, s4: Boolean)

      groupT {
        O.string --"optName" ::
        O.int    --"optInt" ::
        group[Config] {
          string -~ name("name") -~ des("string") ::
          int ::
          string -~ name("name2") ::
          boolean
        }
      }: ConfigDsl[(Option[String], Option[Int], Config)]
    }

    it("allow mixing to case class") {
      case class Config(a1: Option[Int], aa: Option[String], a2: String, a3: Int, a4: String, a5: Boolean)
      group[Config] {
        O.int --"opt1" ::
        O.string --"opt2" ::
        string -~ name("name") -~ des("string") ::
        int ::
        string -~ name("name2") ::
        boolean
      }: ConfigDsl[Config]
    }

    it("config from options") {
      case class Config(a1: Option[Int], aa: Option[String], a3: Boolean)
      group[Config] {
        O.int --"opt1" ::
        O.string --"opt2" ::
        flag --"opt3"
      }: ConfigDsl[Config]
    }

    it("config from single option") {
      O.int --"opt1": ConfigDsl[Option[Int]]
    }

    it("config from arguments") {
      case class Config(a1: Int, aa: String, a3: Boolean)
      group[Config] {
        int -~ name("arg1") ::
        string -~ name("arg2") ::
        boolean -~ name("arg3")
      }: ConfigDsl[Config]
    }

    it("config from single argument") {
      group[Int] {
        int -~ name("arg1")
      }: ConfigDsl[Int]
    }

    it("complex configuration") {
      case class ConfigA(
        a1: String,
        a2: Boolean,
        a3: Option[Int],
        a4: ConfigB,
        a5: Option[String],
        a6: String,
        a7: Boolean,
        a8: Int)

      case class ConfigB(b1: Option[String], b2: Boolean, b3: Option[Int], b4: ConfigC, b5: ConfigD)
      case class ConfigC(c1: Option[String], c2: Int)
      case class ConfigD(d1: Boolean)

      group[ConfigA] {
        O.string --"a1" -'a' -~ des("a1") -~ or("a1") ::
        flag     --"a2" ::
        O.int    --"a3" ::
        sub[ConfigB](des("a4")) {
          O.string --"b1" ::
          flag     --"b2" ::
          opt[Int] --"b3" ::
          sub[ConfigC](des("b4")) {
            O.string --"c1" ::
            O.int    --"c2" -~ or(2)
          } ::
          sub[ConfigD](des("c3")) {
            flag -- "d1"
          }
        } ::
        O.string --"a5" -~ des("a5") ::
        string ::
        boolean ::
        int
      }
    }
  }
}
