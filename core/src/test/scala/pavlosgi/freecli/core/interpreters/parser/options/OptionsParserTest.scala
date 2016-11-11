package pavlosgi.freecli.core.interpreters.parser.options

import shapeless._

import pavlosgi.freecli.core.dsl.options._
import pavlosgi.freecli.testkit.Test

class OptionsParserTest extends Test {
  describe("Arg tests") {
    it("parse string with name") {
      val res = parseOptions(Seq("--host", "localhost"))(string --"host")
      res.valid should === ("localhost")
    }

    it("parse string with abbreviation") {
      val res = parseOptions(Seq("-h", "localhost"))(string -'h')
      res.valid should ===("localhost")
    }

    it("parse string with both name and abbreviation using abbreviation") {
      val res = parseOptions(Seq("-h", "localhost"))(string --"host" -'h')
      res.valid should === ("localhost")
    }

    it("parse string with both name and abbreviation using name") {
      val res = parseOptions(Seq("--host", "localhost"))(string --"host" -'h')
      res.valid should === ("localhost")
    }

    it("fail to parse string using the wrong field name format") {
      val dsl = string --"host" -'h'
      val res = parseOptions(Seq("-host", "localhost"))(dsl)

      res.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
      }.distinct.size should === (1)

      val res1 = parseOptions(Seq("host", "localhost"))(dsl)

      res1.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
        case c: OptionFieldMissingParsingError => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("fail to parse string using the wrong field abbreviation format") {
      val dsl = string --"host" -'h'
      val res = parseOptions(Seq("--h", "localhost"))(dsl)

      println(res.invalid)
      res.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
        case c: OptionFieldMissingParsingError => c.getClass.getName
      }.distinct.size should === (2)

      val res1 = parseOptions(Seq("h", "localhost"))(dsl)

      res1.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
        case c: OptionFieldMissingParsingError => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("fail to parse string if value not provided") {
      val dsl = string --"host" -'h'
      val res = parseOptions(Seq("-h"))(dsl)

      res.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
        case c: OptionFieldValueMissingParsingError => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("parse string with default") {
      val res = parseOptions(Seq())(string --"host" -'h'-~ or("myhost"))
      res.valid should === ("myhost")
    }

    it("parse string with default and override") {
      val res = parseOptions(Seq("--host", "localhost"))(
                  string --"host" -'h' -~ or("myhost"))

      res.valid should === ("localhost")
    }

    it("parse int arg with default") {
      val res = parseOptions(Seq("-p", "8080"))(int --"port" -'p' -~ or(5432))
      res.valid should === (8080)
    }

    it("fail to parse int") {
      val res = parseOptions(Seq("-p", "8080s"))(int --"port" -'p' -~ or(5432))

      res.invalid.toList.collect {
        case c: StringDecoderParsingError => c.getClass.getName
      }.distinct.size should === (1)
    }

    it("parse flag exists") {
      val res = parseOptions(Seq("--debug"))(flag --"debug" -'d')
      res.valid should === (true)
    }

    it("parse flag missing") {
      val res = parseOptions(Seq())(flag --"debug" -'d')
      res.valid should === (false)
    }

    it("parse opt") {
      val res = parseOptions(Seq("--debug", "true"))(optString --"debug" -'d')
      res.valid should === (Some("true"))
    }

    it("parse opt missing") {
      val res = parseOptions(Seq())(optString --"debug" -'d')
      res.valid should === (None)
    }

    it("fail to parse opt") {
      val res = parseOptions(Seq("--debug", "value"))(optInt --"debug" -'d')
      res.invalid.toList.collect {
        case c: StringDecoderParsingError => c.getClass.getName
      }.distinct.size should === (1)

      val res2 = parseOptions(Seq("--debug"))(optInt --"debug" -'d')
      res2.invalid.toList.collect {
        case c: UnknownArgumentsParsingError => c.getClass.getName
        case c: OptionFieldValueMissingParsingError => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("parse tuple string int with name") {
      val res = parseOptions(Seq("--host", "localhost", "--port", "8080"))(
        options[(String, Int)](string --"host" :: int --"port"))

      res.valid should === ("localhost" -> 8080)
    }

    it("parse hlist string int with name") {
      val res = parseOptions(Seq("--host", "localhost", "--port", "8080"))(
        string --"host" :: int --"port")

      res.valid should === ("localhost" :: 8080 :: HNil)
    }

    it("parse arguments to build type") {
      case class ServerConfig(host: String, port: Int, debug: Boolean)

      val dsl =
        options[ServerConfig] {
          string --"host"  ::
          int    --"port"  ::
          flag   --"debug1"
        }

      val res = parseOptions(Seq("--host", "localhost", "--port", "8080", "--debug1"))(dsl)
      res.valid should === (ServerConfig("localhost", 8080, true))
    }

    it("parse arguments to build type with subconfiguration") {
      case class DbConfig(dbHost: String, dbPort: Int)
      case class ServerConfig(host: String, port: Int, debug: Boolean, dbConfig: DbConfig)

      val dsl =
        options[ServerConfig] {
          string --"host"  ::
          int    --"port"  ::
          flag   --"debug" ::
          sub[DbConfig]("Database configuration") {
            string --"dbhost" ::
            int    --"dbport"
          }
        }

      val res = parseOptions(
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
        options[A] {
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

      parseOptions(args)(dsl).valid should === (
        A(
          "aString",
          aFlag = true,
          1,
          B(
            "bString",
            bFlag = true,
            Some(1),
            C(
              "cString",
              1),
            D(true)),
          "aString2"))
    }

    it("should allow passing multiple field abbreviations under a single slash for flags") {
      case class Flags(first: Boolean, second: Boolean, third: Boolean, fourth: Boolean)

      val dsl =
        options[Flags] {
          flag   - 'p' ::
          flag   - 'n' ::
          flag   - 's' ::
          flag   - 't'
        }

      parseOptions(Seq("-pnst"))(dsl).valid should === (
        Flags(first = true, second = true, third = true, fourth = true))

      case class Flags2(first: Boolean, second: Boolean, third: String)

      val dsl2 =
        options[Flags2] {
          flag   - 'p' ::
          flag   - 'n' ::
          string - 's'
        }

      parseOptions(Seq("-pn", "-s", "string"))(dsl2).valid should === (
        Flags2(true, true, "string"))

    }

    it("should escape multiple field abbreviation split") {
      case class A(first: String)

      val dsl =
        options[A] {
          string - 'p'
        }

      parseOptions(Seq("-p", "-host"))(dsl).valid should === (A("-host"))
    }

    it("should throw a runtime exception for bad field names") {
      assertThrows[IllegalArgumentException](string --"--host")
      assertThrows[IllegalArgumentException](string --"123")
      assertThrows[IllegalArgumentException](string -'0')
    }
  }
}
