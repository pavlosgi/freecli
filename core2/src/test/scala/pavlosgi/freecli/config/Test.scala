package pavlosgi
package freecli
package config

import java.io.File

import cats.std.list._

import pavlosgi.freecli.config.all._

class Test extends testkit.Test {
  describe("Parser") {

    it("parse arg") {
      val c = parse(Seq("--logging", "true"))(arg[Boolean]("logging"))
      c.valid should === (true)

      val c1 = parse(Seq("--logging", "false"))(arg[Boolean]("logging"))
      c1.valid should === (false)

      val c3 = parse(Seq("--logging", "1"))(arg[Boolean]("logging"))
      c3.invalid.head shouldBe a [InvalidValueType]

      val c4 = parse(Seq("--host", "localhost"))(string("host"))
      c4.valid should === ("localhost")

      val c5 = parse(Seq("--host"))(string("host"))
      c5.invalid.map(_.getClass) should contain
        theSameElementsAs (List(InvalidArgs.getClass, FieldValueMissing.getClass))

      val c6 = parse(Seq("--host2", "localhost"))(string("host"))
      c6.invalid.map(_.getClass) should contain
        theSameElementsAs (List(InvalidArgs.getClass, FieldMissingAndNoDefault.getClass))
    }

    it("parse arg with defaults") {
      val c = parse(Seq())(arg[Boolean]("logging", default = Some(true)))
      c.valid should === (true)

      val c1 = parse(Seq())(arg[String]("host", default = Some("localhost")))
      c1.valid should === ("localhost")

      val c2 = parse(Seq("--host"))(arg[String]("host", default = Some("localhost")))
      c2.invalid.map(_.getClass) should contain
        theSameElementsAs (List(InvalidArgs.getClass, FieldValueMissing.getClass))
    }

    it("parse opt") {
      val c = parse(Seq.empty)(opt[Boolean]("logging"))
      c.valid should be (empty)

      val c1 = parse(Seq("--port", "5432"))(opt[Int]("port"))
      c1.valid.get should === (5432)

      val c2 = parse(Seq("--port"))(opt[Int]("port"))
      c2.invalid.map(_.getClass) should contain
        theSameElementsAs (List(InvalidArgs.getClass, FieldValueMissing.getClass))
    }

    it("parse file") {
      val c = parse(Seq("--file",
                        s"${getClass.getResource("/file.txt").getFile}")

                   )(arg[File]("file"))

      c.valid should === (new File(getClass.getResource("/file.txt").getFile))
    }

    it("parse mixed args") {
      case class AuthConfig(port: Int, host: String)
      case class ServerConfig(logging: Boolean, auth: AuthConfig)

      val authConfig = (int("port") |@| string("host")).map(AuthConfig)
      val serverConfig =
        (boolean("logging") |@| sub("auth")(authConfig)).map(ServerConfig)

      val c = parse(Seq("--logging",
                        "true",
                        "auth",
                        "--host",
                        "localhost",
                        "--port",
                        "5432"))(serverConfig)

      c.valid should === (ServerConfig(true, AuthConfig(5432, "localhost")))
    }

    it("parse nested subs") {
      case class DBConfig(dbPort: Int, dbHost: String)
      case class AuthConfig(port: Int, host: String, dbConfig: DBConfig)
      case class ServerConfig(logging: Boolean, auth: AuthConfig)

      val dbConfig = (int("db-port") |@| string("db-host")).map(DBConfig)
      val authConfig = (int("port") |@| string("host") |@| sub("db")(dbConfig)).map(AuthConfig)
      val serverConfig =
        (boolean("logging") |@| sub("auth")(authConfig)).map(ServerConfig)

      val c = parse(Seq("--logging",
                        "true",
                        "auth",
                        "--host",
                        "localhost",
                        "--port",
                        "8080",
                        "db",
                        "--db-host",
                        "localhost",
                        "--db-port",
                        "5432"))(serverConfig)

      c.valid should === (ServerConfig(true, AuthConfig(8080, "localhost", DBConfig(5432, "localhost"))))
    }

    it("fail to parse nested subs") {
      case class DBConfig(dbPort: Int, dbHost: String)
      case class AuthConfig(port: Int, host: String, dbConfig: DBConfig)
      case class ServerConfig(logging: Boolean, auth: AuthConfig)

      val dbConfig = (int("db-port") |@| string("db-host")).map(DBConfig)
      val authConfig = (int("port") |@| string("host") |@| sub("db")(dbConfig)).map(AuthConfig)
      val serverConfig =
        (boolean("logging") |@| sub("auth")(authConfig)).map(ServerConfig)

      val c = parse(Seq("--logging",
                        "true",
                        "db",
                        "--db-host",
                        "localhost",
                        "--db-port",
                        "5432",
                        "auth",
                        "--host",
                        "localhost",
                        "--port",
                        "8080"))(serverConfig)

      c.invalid.map(_.getClass) should contain
        theSameElementsAs (List(InvalidArgs.getClass, SubFieldMissing.getClass))
    }

    it("parse multiple subs") {
      case class DBConfig(dbPort: Int, dbHost: String)
      case class AuthConfig(port: Int, host: String)
      case class ServerConfig(logging: Boolean, auth: AuthConfig, db: DBConfig)

      val dbConfig = (int("db-port") |@| string("db-host")).map(DBConfig)
      val authConfig = (int("port") |@| string("host")).map(AuthConfig)
      val serverConfig =
        (boolean("logging") |@| sub("auth")(authConfig) |@| sub("db")(dbConfig))
          .map(ServerConfig)

      val c = parse(Seq("--logging",
                        "true",
                        "db",
                        "--db-host",
                        "localhost",
                        "--db-port",
                        "5432",
                        "auth",
                        "--host",
                        "localhost",
                        "--port",
                        "8080"))(serverConfig)

      c.valid should === (ServerConfig(true,
                                       AuthConfig(8080, "localhost"),
                                       DBConfig(5432, "localhost")))
    }

    it("fail to parse multiple subs") {
      case class DBConfig(dbPort: Int, dbHost: String)
      case class AuthConfig(port: Int, host: String)
      case class ServerConfig(logging: Boolean, auth: AuthConfig, db: DBConfig)

      val dbConfig = (int("db-port") |@| string("db-host")).map(DBConfig)
      val authConfig = (int("port") |@| string("host")).map(AuthConfig)
      val serverConfig =
        (boolean("logging") |@| sub("auth")(authConfig) |@| sub("db")(dbConfig))
          .map(ServerConfig)

      val c = parse(Seq("--logging",
                        "true",
                        "db",
                        "--db-host",
                        "localhost",
                        "--db-port",
                        "5432",
                        "auth",
                        "--host",
                        "localhost",
                        "--port2",
                        "8080"))(serverConfig)

      c.invalid.map(_.getClass) should contain
        theSameElementsAs (List(InvalidArgs.getClass, SubFieldMissing.getClass))
    }

    it("generate help") {
      val configDsl = getConfigDsl
      usage(configDsl) should not be empty
    }
  }

  case class AuthConfig(port: Int, host: String)
  case class ServerConfig(logging: Boolean, auth: AuthConfig)

  def getConfigDsl: ConfigDsl[ParserShow, ServerConfig] = {
    val authConfig = (int("port") |@| string("host")).map(AuthConfig)
    (boolean("logging") |@| sub("auth")(authConfig)).map(ServerConfig)
  }
}
