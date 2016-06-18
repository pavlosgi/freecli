package pavlosgi
package freecli
package core

import dsl._
import parser._

import java.io.File

import cats.syntax.cartesian._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest._

class Test extends FunSpec with Matchers with TypeCheckedTripleEquals {
  describe("Parser") {

    it("parse boolean") {
      val c = parse(Seq("--logging"))(boolean("logging"))
      c.toOption.get should === (true)

      val c1 = parse(Seq("--logging=false"))(boolean("logging"))
      c1.toOption.get should === (false)

      val c2 = parse(Seq.empty)(boolean("logging"))
      c2.toOption.get should === (false)

      val c3 = parse(Seq("--logging=1"))(boolean("logging"))
      c3.invalid.head.isInstanceOf[InvalidValueType] should be (true)
    }

    it("parse string") {
      val c = parse(Seq("--host=localhost"))(string("host"))
      c.toOption.get should === ("localhost")

      val c1 = parse(Seq("--host="))(string("host"))
      c1.invalid.head.isInstanceOf[FieldMissing] should === (true)

      val c2 = parse(Seq("--host"))(string("host"))
      c2.invalid.head.isInstanceOf[FieldMissing] should === (true)

      val c3 = parse(Seq("--host2=localhost"))(string("host"))
      c3.invalid.head.isInstanceOf[FieldMissing] should === (true)
    }

    it("parse int") {
      val c = parse(Seq("--port=5432"))(int("port"))
      c.toOption.get should === (5432)

      val c1 = parse(Seq("--port=localhost"))(int("port"))
      c1.invalid.head.isInstanceOf[InvalidValueType] should be(true)

      val c2 = parse(Seq("--por=localhost"))(int("port"))
      c2.invalid.head.isInstanceOf[FieldMissing] should be(true)
    }

    it("parse file") {
      val c = parse(Seq(s"--file=${getClass.getResource("/file.txt").getFile}"))(file("file"))
      c.toOption.get should === (new File(getClass.getResource("/file.txt").getFile))
    }

    it("parse mixed args") {
      case class AuthConfig(port: Int, host: String)
      case class ServerConfig(logging: Boolean, auth: AuthConfig)

      val authConfig = (int("port") |@| string("host")).map(AuthConfig)
      val serverConfig: ConfigDsl[ServerConfig] =
        (boolean("logging") |@| sub("auth")(authConfig)).map(ServerConfig)

      val c = parse(Seq("--host=localhost", "--port=5432", "--logging"))(serverConfig)
      c.toOption.get should === (ServerConfig(true, AuthConfig(5432, "localhost")))
    }
  }
}
