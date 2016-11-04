package pavlosgi.freecli.core.config.interpreters

import cats.std.all._
import cats.syntax.all._
import shapeless._

import pavlosgi.freecli.core.dsl.config._
import pavlosgi.freecli.core.config.interpreters.parser._
import pavlosgi.freecli.testkit.Test

class ParserTest extends Test {
  describe("Arg tests") {
    it("parse string with name") {
      val res = parseConfig(Seq("--host", "localhost"))(string --"host")
      res.valid should === ("localhost")
    }

    it("parse string with abbreviation") {
      val res = parseConfig(Seq("-h", "localhost"))(string -'h')
      res.valid should ===("localhost")
    }

    it("parse string with both name and abbreviation using abbreviation") {
      val res = parseConfig(Seq("-h", "localhost"))(string --"host" -'h')
      res.valid should === ("localhost")
    }

    it("parse string with both name and abbreviation using name") {
      val res = parseConfig(Seq("--host", "localhost"))(string --"host" -'h')
      res.valid should === ("localhost")
    }

    it("fail to parse string using the wrong field name format") {
      val dsl = string --"host" -'h'
      val res = parseConfig(Seq("-host", "localhost"))(dsl)

      res.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
        case c: ConfigFieldValueMissingParsingError => c.getClass.getName
      }.distinct.size should === (2)

      val res1 = parseConfig(Seq("host", "localhost"))(dsl)

      res1.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
        case c: ConfigFieldValueMissingParsingError => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("fail to parse string using the wrong field abbreviation format") {
      val dsl = string --"host" -'h'
      val res = parseConfig(Seq("--h", "localhost"))(dsl)

      res.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
        case c: ConfigFieldValueMissingParsingError => c.getClass.getName
      }.distinct.size should === (2)

      val res1 = parseConfig(Seq("h", "localhost"))(dsl)

      res1.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
        case c: ConfigFieldValueMissingParsingError => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("parse string with default") {
      val res = parseConfig(Seq())(string --"host" -'h'-| "myhost")
      res.valid should === ("myhost")
    }

    it("parse string with default and override") {
      val res = parseConfig(Seq("--host", "localhost"))(
                  string --"host" -'h' -| "myhost")

      res.valid should === ("localhost")
    }

    it("parse int arg with default") {
      val res = parseConfig(Seq("-p", "8080"))(int --"port" -'p' -| 5432)
      res.valid should === (8080)
    }

    it("fail to parse int") {
      val res = parseConfig(Seq("-p", "8080s"))(int --"port" -'p' -| 5432)

      res.invalid.toList.collect {
        case c: StringDecoderParsingError => c.getClass.getName
      }.distinct.size should === (1)
    }

    it("parse flag with false value") {
      val res = parseConfig(Seq("--debug", "false"))(flag --"debug" -'d')
      res.valid should === (false)
    }

    it("parse flag with true value") {
      val res = parseConfig(Seq("--debug", "true"))(flag --"debug" -'d')
      res.valid should === (true)
    }

    it("parse missing flag without default to false") {
      val res = parseConfig(Seq())(flag --"debug" -'d')
      res.valid should === (false)
    }

    it("parse flag with default and override") {
      val res = parseConfig(Seq("--debug"))(flag --"debug" -'d' -| false)
      res.valid should === (true)
    }

    it("fail to parse flag") {
      val res = parseConfig(Seq("--debug", "truee"))(flag --"debug" -'d' -| false)
      res.invalid.toList.collect {
        case c: StringDecoderParsingError => c.getClass.getName
      }.distinct.size should === (1)
    }

    it("parse opt") {
      val res = parseConfig(Seq("--debug", "true"))(optString --"debug" -'d')
      res.valid should === (Some("true"))
    }

    it("parse opt missing") {
      val res = parseConfig(Seq())(optString --"debug" -'d')
      res.valid should === (None)
    }

    it("fail to parse opt") {
      val res = parseConfig(Seq("--debug", "value"))(optInt --"debug" -'d')
      res.invalid.toList.collect {
        case c: StringDecoderParsingError => c.getClass.getName
      }.distinct.size should === (1)
    }


    it("parse tuple string int with name") {
      val res = parseConfig(Seq("--host", "localhost", "--port", "8080"))(
        string --"host" :-> int --"port")

      res.valid should === ("localhost" -> 8080)
    }

    it("parse hlist string int with name") {
      val res = parseConfig(Seq("--host", "localhost"))(
        string --"host" :: int --"port")

      res.valid should === ("localhost" :: 8080 :: HNil)
    }

    it("parse arguments to build type") {
      case class ServerConfig(host: String, port: Int, debug: Boolean)

      val dsl =
        config[ServerConfig] {
          string --"host"  ::
          int    --"port"  ::
          flag   --"debug1"
        }

      val res = parseConfig(Seq("--host", "localhost", "--port", "8080", "--debug1"))(dsl)
      res.valid should === (ServerConfig("localhost", 8080, true))
    }

    it("parse arguments to build type with subconfiguration") {
      case class DbConfig(dbHost: String, dbPort: Int)
      case class ServerConfig(host: String, port: Int, debug: Boolean, dbConfig: DbConfig)

      val dsl =
        config[ServerConfig] {
          string --"host"  ::
          int    --"port"  ::
          flag   --"debug" ::
          sub[DbConfig]("Database configuration") {
            string --"dbhost" ::
            int    --"dbport"
          }
        }

      val res = parseConfig(
                  Seq(
                    "--host",
                    "localhost",
                    "--port",
                    "8080",
                    "--debug",
                    "--dbhost",
                    "postgresql",
                    "--dbport",
                    "5432"))(dsl)

      res.valid should === (ServerConfig(
                              "localhost",
                              8080,
                              true,
                              DbConfig("postgresql", 5432)))
    }

    it("parse complex configuration") {
      case class A(aString: String, aFlag: Boolean, aInt: Int, b: B, aString2: String)
      case class B(bString: String, bFlag: Boolean, bOptInt: Option[Int], c: C, d: D)
      case class C(cString: String, cInt: Int)
      case class D(dFlag: Boolean)

      val dsl =
        config[A] {
          string --"aString" ::
          flag   --"aFlag"  ::
          int    --"aInt"   ::
          sub[B]("b") {
            string --"bString" ::
            flag   --"bFlag"   ::
            optInt --"bOptInt" ::
            sub[C]("c") {
              string --"cString" ::
              int    --"cInt"
            } ::
            sub[D]("d") {
              flag -- "dFlag"
            }
          } ::
          string --"aString2"
        }

      val args = Seq(
        "--aString",
        "aString",
        "--aFlag",
        "--aInt",
        "1",
        "--bString",
        "bString",
        "--bFlag",
        "--bOptInt",
        "1",
        "--cString",
        "cString",
        "--cInt",
        "1",
        "--dFlag",
        "--aString2",
        "aString2")

      parseConfig(args)(dsl).valid should === (
        A(
          "aString",
          true,
          1,
          B(
            "bString",
            true,
            Some(1),
            C(
              "cString",
              1),
            D(true)),
          "aString2"))
    }

    it("should allow passing multiple field abbreviations under a single slash for flags") {
      case class Flags(first: Boolean, second: Boolean, third: Boolean)

      val dsl =
        config[Flags] {
          flag - 'p' ::
          flag - 'n' ::
          flag - 's'
        }

      parseConfig(Seq("-pns"))(dsl).valid should === (Flags(true, true, true))

      case class Flags2(first: Boolean, second: Boolean, third: String)

      val dsl2 =
        config[Flags2] {
          flag   - 'p' ::
          flag   - 'n' ::
          string - 's'
        }

      parseConfig(Seq("-pn", "-s", "string"))(dsl2).valid should === (
        Flags2(true, true, "string"))

    }

    it("should throw a runtime exception for bad field names (hopefully we can make this static as well)") {
      assertThrows[IllegalArgumentException](string --"--host")
      assertThrows[IllegalArgumentException](string --"123")
      assertThrows[IllegalArgumentException](string -'0')
    }
  }
}
