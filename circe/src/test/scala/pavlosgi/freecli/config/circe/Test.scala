package pavlosgi
package freecli
package config
package circe

import all._

import cats.Show
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Json}
import org.scalatest.{FunSpec, Matchers}

class Test extends FunSpec with Matchers {
  describe("circe") {
    it("decode json files") {
      case class Foo(foo: String, bar: Bar)
      case class Bar(bar: String)

      implicit val s = new Show[Foo] {
        override def show(f: Foo): String = ""
      }

      implicit val d = implicitly[Decoder[Foo]]
      val c = parseConfig(Seq(s"--json=${getClass.getResource("/file.json").getFile}"))(arg[Foo]("json", "json file"))
      c.getOrElse(throw new Exception("Invalid")) should === (Foo("foo", Bar("bar")))

      val c1 = parseConfig(Seq(s"--json=${getClass.getResource("/file.json").getFile}"))(arg[Json]("json", "json file"))
      c1.getOrElse(throw new Exception("Invalid")) should === (Foo("foo", Bar("bar")).asJson)
    }
  }
}
