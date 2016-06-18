package pavlosgi
package freecli
package circe

import core.dsl._
import core.parser._

import cats.Show
import io.circe.Decoder
import io.circe.syntax._

import org.scalatest.{FunSpec, Matchers}
import io.circe.generic.auto._

class Test extends FunSpec with Matchers {
  describe("circe") {
    it("decode json files") {
      case class Foo(foo: String, bar: Bar)
      case class Bar(bar: String)

      implicit val s = new Show[Foo] {
        override def show(f: Foo): String = ""
      }

      implicit val d = implicitly[Decoder[Foo]]
      val c = parse(Seq(s"--json=${getClass.getResource("/file.json").getFile}"))(arg[Foo]("json", "json file"))
      c.getOrElse(throw new Exception("Invalid")) should === (Foo("foo", Bar("bar")))

      val c1 = parse(Seq(s"--json=${getClass.getResource("/file.json").getFile}"))(arg("json", "json file"))
      c1.getOrElse(throw new Exception("Invalid")) should === (Foo("foo", Bar("bar")).asJson)
    }
  }
}
