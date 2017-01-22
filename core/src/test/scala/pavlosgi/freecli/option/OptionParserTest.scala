package pavlosgi.freecli.option

import shapeless._

import pavlosgi.freecli.core.all._
import pavlosgi.freecli.option.api.{HelpAction, VersionAction}
import pavlosgi.freecli.option.all._
import pavlosgi.freecli.testkit.Test
import pavlosgi.freecli.option.parser._
import pavlosgi.freecli.Helpers._

class OptionParserTest extends Test {
  describe("Options parser") {
    it("parse string option with name") {
      val res = parseOption(string --"host").run(Seq("--host", "localhost"))
      res.success should === (Some("localhost"))
    }

    it("parse option if missing") {
      val res = parseOption(string --"debug" -'d').run(Seq())
      res.success should === (None)
    }

    it("parse string option with abbreviation") {
      val res = parseOption(string -'h').run(Seq("-h", "localhost"))
      res.success should === (Some("localhost"))
    }

    it("parse string option with both name and abbreviation using abbreviation") {
      val res = parseOption(string --"host" -'h').run(Seq("-h", "localhost"))
      res.success should === (Some("localhost"))
    }

    it("parse string option with both name and abbreviation using name") {
      val res = parseOption(string --"host" -'h').run(Seq("--host", "localhost"))
      res.success should === (Some("localhost"))
    }

    it("fail to parse string option using the wrong field name format") {
      val dsl = string --"host" -'h' -~ req
      val res = parseOption(dsl).run(Seq("-host", "localhost"))

      res.failure.toList.collect {
        case c: AdditionalArgumentsFound => c.getClass.getName
      }.distinct.size should === (1)

      val res1 = parseOption(dsl).run(Seq("host", "localhost"))

      res1.failure.toList.collect {
        case c: AdditionalArgumentsFound => c.getClass.getName
        case c: OptionFieldMissing => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("fail to parse string option using the wrong field abbreviation format") {
      val dsl = string --"host" -'h' -~ req
      val res = parseOption(dsl).run(Seq("--h", "localhost"))

      res.failure.toList.collect {
        case c: AdditionalArgumentsFound => c.getClass.getName
        case c: OptionFieldMissing => c.getClass.getName
      }.distinct.size should === (2)

      val res1 = parseOption(dsl).run(Seq("h", "localhost"))

      res1.failure.toList.collect {
        case c: AdditionalArgumentsFound => c.getClass.getName
        case c: OptionFieldMissing => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("fail to parse string option if value not provided") {
      val dsl = string --"host" -'h'
      val res = parseOption(dsl).run(Seq("-h"))
      res.failure.toList.collect {
        case c: AdditionalArgumentsFound => c.getClass.getName
        case c: OptionFieldValueMissing => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("parse string option with default") {
      val res = parseOption(string --"host" -'h'-~ or("myhost")).run(Seq())
      res.success should === ("myhost")
    }

    it("parse string option with default and override") {
      val res = parseOption(string --"host" -'h' -~ or("myhost"))
        .run(Seq("--host", "localhost"))

      res.success should === ("localhost")
    }

    it("parse int option with default") {
      val res = parseOption(int --"port" -'p' -~ or(5432)).run(Seq("-p", "8080"))
      res.success should === (8080)
    }

    it("fail to parse int option") {
      val res = parseOption(int --"port" -'p' -~ or(5432)).run(Seq("-p", "8080s"))

      res.failure.toList.collect {
        case c: FailedToDecodeOption => c.getClass.getName
      }.distinct.size should === (1)
    }

    it("parse flag option") {
      val res = parseOption(flag --"debug" -'d').run(Seq("--debug"))
      res.success should === (true)
    }

    it("parse flag option if flag is missing") {
      val res = parseOption(flag --"debug" -'d').run(Seq())
      res.success should === (false)
    }

    it("parse required option") {
      val res = parseOption(string --"debug" -'d' -~ req).run(Seq("--debug", "true"))
      res.success should === ("true")
    }

    it("parse required option missing") {
      val res = parseOption(string --"debug" -'d').run(Seq())
      res.success should === (None)
    }

    it("fail to parse required option") {
      val res = parseOption(int --"debug" -'d').run(Seq("--debug", "value"))
      res.failure.toList.collect {
        case c: FailedToDecodeOption => c.getClass.getName
      }.distinct.size should === (1)

      val res2 = parseOption(int --"debug" -'d').run(Seq("--debug"))
      res2.failure.toList.collect {
        case c: AdditionalArgumentsFound => c.getClass.getName
        case c: OptionFieldValueMissing => c.getClass.getName
      }.distinct.size should === (2)
    }

    it("parse tuple string int with name") {
      val res = parseOption(groupT(string --"host" :: int --"port"))
        .run(Seq("--host", "localhost", "--port", "8080"))

      res.success should === (Some("localhost") -> Some(8080))
    }

    it("parse hlist string int with name") {
      val res = parseOption(string --"host" :: int --"port")
        .run(Seq("--host", "localhost", "--port", "8080"))

      res.success should === (Some("localhost") :: Some(8080) :: HNil)
    }

    it("parse options to build type") {
      case class ServerConfig(host: Option[String], port: Option[Int], debug: Boolean)

      val dsl =
        group[ServerConfig] {
          string --"host"  ::
          int    --"port"  ::
          flag   --"debug1"
        }

      val res = parseOption(dsl)
        .run(Seq("--host", "localhost", "--port", "8080", "--debug1"))
      res.success should === (ServerConfig(Some("localhost"), Some(8080), true))
    }

    it("parse options to build type with subconfiguration") {
      case class DbConfig(dbHost: String, dbPort: Int)
      case class ServerConfig(host: String, port: Int, debug: Boolean, dbConfig: DbConfig)

      val dsl =
        group[ServerConfig] {
          string --"host" -~ req ::
          int   --"port" -~ req ::
          flag   --"debug" ::
          sub[DbConfig](des("Database configuration")) {
            string --"dbhost" -~ req ::
            int    --"dbport" -~ req
          }
        }

      val res = parseOption(dsl)
        .run(Seq(
          "--host",
          "localhost",
          "--port",
          "8080",
          "--debug",
          "--dbhost",
          "postgresql",
          "--dbport",
          "5432"))

      res.success should === (ServerConfig(
                              "localhost",
                              8080,
                              true,
                              DbConfig("postgresql", 5432)))
    }

    it("should allow passing multiple field abbreviations under a single slash for flags") {
      case class Flags(first: Boolean, second: Boolean, third: Boolean, fourth: Boolean)

      val dsl =
        group[Flags] {
          flag   - 'p' ::
          flag   - 'n' ::
          flag   - 's' ::
          flag   - 't'
        }

      parseOption(dsl).run(Seq("-pnst")).success should === (
        Flags(first = true, second = true, third = true, fourth = true))

      case class Flags2(first: Boolean, second: Boolean, third: Option[String])

      val dsl2 =
        group[Flags2] {
          flag   - 'p' ::
          flag   - 'n' ::
          string - 's'
        }

      parseOption(dsl2).run(Seq("-pn", "-s", "string")).success should === (
        Flags2(true, true, Some("string")))
    }

    it("should escape multiple field abbreviation split") {
      case class A(first: String)

      val dsl =
        group[A] {
          string -'p' -~ req
        }

      parseOption(dsl).run(Seq("-p", "-host")).success should === (A("-host"))
    }

    it("parse options and display help") {
      case class A(first: String)

      val dsl =
        group[A] {
          string   -'p' -~ req ::
          ops.help --"help"
        }

      val action = parseOption(dsl).run(Seq("--help")).action

      Option(action).collect {
        case _: HelpAction.type => true
      }.exists(identity) should === (true)
    }

    it("parse options and display version") {
      case class A(first: String)

      val dsl =
        group[A] {
          string   -'p' -~ req ::
          ops.version --"version" -~ ops.value("1.0")
        }

      val action = parseOption(dsl).run(Seq("--version")).action

      Option(action).collect {
        case _: VersionAction => true
      }.exists(identity) should === (true)
    }

    it("should throw a runtime exception for bad field names") {
      assertThrows[IllegalArgumentException](string --"--host")
      assertThrows[IllegalArgumentException](string --"123")
      assertThrows[IllegalArgumentException](string -'0')
    }
  }
}
