package pavlosgi.freecli.core.interpreters.parser.config

import shapeless._

import pavlosgi.freecli.testkit.Test
import pavlosgi.freecli.core.dsl.config._

class ConfigParserTest extends Test {
  describe("Config tests") {

    describe("Argument tests") {
      it("parse int") {
        val res = parseConfig(Seq("1"))(int)
        res.valid should === (1)
      }

      it("parse multiple tuple") {
        val res = parseConfig(Seq("c1", "2", "c3"))(
          config {
            string ::
            int    ::
            string
          })

        res.valid should === (("c1", 2, "c3"))
      }

      it("parse multiple case class") {
        case class A(c1: String, c2: Int, c3: String)
        val res = parseConfig(Seq("c1", "2", "c3"))(
          config[A] {
            string ::
            int    ::
            string
          })

        res.valid should === (A("c1", 2, "c3"))
      }
    }

    describe("Option tests") {
      it("parse string with name") {
        val res = parseConfig(Seq("--host", "localhost"))(o.string --"host")
        res.valid should === ("localhost")
      }

      it("parse string with abbreviation") {
        val res = parseConfig(Seq("-h", "localhost"))(o.string -'h')
        res.valid should ===("localhost")
      }

      it("parse string with both name and abbreviation using abbreviation") {
        val res = parseConfig(Seq("-h", "localhost"))(o.string --"host" -'h')
        res.valid should === ("localhost")
      }

      it("parse string with both name and abbreviation using name") {
        val res = parseConfig(Seq("--host", "localhost"))(o.string --"host" -'h')
        res.valid should === ("localhost")
      }

      it("fail to parse string using the wrong field name format") {
        val dsl = o.string --"host" -'h'
        val res = parseConfig(Seq("-host", "localhost"))(dsl)

        res.invalid.toList.collect {
          case c: UnknownArgumentsParsingError => c.getClass.getName
        }.distinct.size should === (1)

        val res1 = parseConfig(Seq("host", "localhost"))(dsl)

        res1.invalid.toList.collect {
          case c: UnknownArgumentsParsingError => c.getClass.getName
          case c: OptionFieldMissingParsingError => c.getClass.getName
        }.distinct.size should === (2)
      }

      it("fail to parse string using the wrong field abbreviation format") {
        val dsl = o.string --"host" -'h'
        val res = parseConfig(Seq("--h", "localhost"))(dsl)

        res.invalid.toList.collect {
          case c: UnknownArgumentsParsingError => c.getClass.getName
          case c: OptionFieldMissingParsingError => c.getClass.getName
        }.distinct.size should === (2)

        val res1 = parseConfig(Seq("h", "localhost"))(dsl)

        res1.invalid.toList.collect {
          case c: UnknownArgumentsParsingError => c.getClass.getName
          case c: OptionFieldMissingParsingError => c.getClass.getName
        }.distinct.size should === (2)
      }

      it("fail to parse string if value not provided") {
        val dsl = o.string --"host" -'h'
        val res = parseConfig(Seq("-h"))(dsl)

        res.invalid.toList.collect {
          case c: UnknownArgumentsParsingError => c.getClass.getName
          case c: OptionFieldValueMissingParsingError => c.getClass.getName
        }.distinct.size should === (2)
      }

      it("parse string with default") {
        val res = parseConfig(Seq())(o.string --"host" -'h'-~ or("myhost"))
        res.valid should === ("myhost")
      }

      it("parse string with default and override") {
        val res = parseConfig(Seq("--host", "localhost"))(
                    o.string --"host" -'h' -~ or("myhost"))

        res.valid should === ("localhost")
      }

      it("parse int arg with default") {
        val res = parseConfig(Seq("-p", "8080"))(o.int --"port" -'p' -~ or(5432))
        res.valid should === (8080)
      }

      it("fail to parse int") {
        val res = parseConfig(Seq("-p", "8080s"))(o.int --"port" -'p' -~ or(5432))

        res.invalid.toList.collect {
          case c: StringDecoderParsingError => c.getClass.getName
        }.distinct.size should === (1)
      }

      it("parse flag exists") {
        val res = parseConfig(Seq("--debug"))(flag --"debug" -'d')
        res.valid should === (true)
      }

      it("parse flag missing") {
        val res = parseConfig(Seq())(flag --"debug" -'d')
        res.valid should === (false)
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

        val res2 = parseConfig(Seq("--debug"))(optInt --"debug" -'d')
        res2.invalid.toList.collect {
          case c: UnknownArgumentsParsingError => c.getClass.getName
          case c: OptionFieldValueMissingParsingError => c.getClass.getName
        }.distinct.size should === (2)
      }

      it("parse tuple string int with name") {
        val res = parseConfig(Seq("--host", "localhost", "--port", "8080"))(
          config[(String, Int)](o.string --"host" :: o.int --"port"))

        res.valid should === ("localhost" -> 8080)
      }

      it("parse hlist string int with name") {
        val res = parseConfig(Seq("--host", "localhost", "--port", "8080"))(
          o.string --"host" :: o.int --"port")

        res.valid should === ("localhost" :: 8080 :: HNil)
      }

      it("parse options to build type") {
        case class ServerConfig(host: String, port: Int, debug: Boolean)

        val dsl =
          config[ServerConfig] {
            o.string --"host"  ::
            o.int    --"port"  ::
            flag   --"debug1"
          }

        val res = parseConfig(Seq("--host", "localhost", "--port", "8080", "--debug1"))(dsl)
        res.valid should === (ServerConfig("localhost", 8080, true))
      }

      it("parse options to build type with subconfiguration") {
        case class DbConfig(dbHost: String, dbPort: Int)
        case class ServerConfig(host: String, port: Int, debug: Boolean, dbConfig: DbConfig)

        val dsl =
          config[ServerConfig] {
            o.string --"host"  ::
            o.int    --"port"  ::
            flag   --"debug" ::
            sub[DbConfig]("Database configuration") {
              o.string --"dbhost" ::
              o.int    --"dbport"
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

      it("should allow passing multiple field abbreviations under a single slash for flags") {
        case class Flags(first: Boolean, second: Boolean, third: Boolean, fourth: Boolean)

        val dsl =
          config[Flags] {
            flag   - 'p' ::
            flag   - 'n' ::
            flag   - 's' ::
            flag   - 't'
          }

        parseConfig(Seq("-pnst"))(dsl).valid should === (
          Flags(first = true, second = true, third = true, fourth = true))

        case class Flags2(first: Boolean, second: Boolean, third: String)

        val dsl2 =
          config[Flags2] {
            flag   - 'p' ::
            flag   - 'n' ::
            o.string - 's'
          }

        parseConfig(Seq("-pn", "-s", "string"))(dsl2).valid should === (
          Flags2(true, true, "string"))

      }

      it("should escape multiple field abbreviation split") {
        case class A(first: String)

        val dsl =
          config[A] {
            o.string - 'p'
          }

        parseConfig(Seq("-p", "-host"))(dsl).valid should === (A("-host"))
      }

      it("should throw a runtime exception for bad field names") {
        assertThrows[IllegalArgumentException](o.string --"--host")
        assertThrows[IllegalArgumentException](o.string --"123")
        assertThrows[IllegalArgumentException](o.string -'0')
      }
    }

    it("parse complex configuration") {
      case class A(
        a1: String,
        a2: Boolean,
        a3: Int,
        a4: B,
        a5: String,
        a6: String,
        a7: Int,
        a8: Boolean)

      case class B(
        b1: String,
        b2: Boolean,
        b3: Option[Int],
        b4: C,
        b5: D)

      case class C(c1: String, c2: Int)
      case class D(d1: Boolean)

      val dsl =
        config[A] {
          o.string --"a1" ::
          flag     --"a2"  ::
          o.int    --"a3"   ::
          sub[B]("b") {
            o.string --"b1" ::
            flag   --"b2"   ::
            optInt --"b3" ::
            sub[C]("c") {
              o.string --"c1" ::
              o.int    --"c2"
            } ::
            sub[D]("d") {
              flag -- "d1"
            }
          } ::
          o.string --"a5" ::
          string ::
          int ::
          boolean
        }

      val args = Seq(
        "--a1",
        "a1",
        "--a2",
        "--a3",
        "3",
        "--b1",
        "b1",
        "--b2",
        "--b3",
        "3",
        "--c1",
        "c1",
        "--c2",
        "2",
        "--d1",
        "--a5",
        "a5",
        "a6",
        "7",
        "true")

      parseConfig(args)(dsl).valid should === (
        A(
          "a1",
          true,
          3,
          B("b1", true, Some(3), C("c1", 2), D(true)),
          "a5",
          "a6",
          7,
          true))
    }
  }
}
