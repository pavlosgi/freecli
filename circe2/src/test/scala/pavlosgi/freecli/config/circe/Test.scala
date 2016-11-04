package pavlosgi
package freecli
package config
package circe

import freecli.config.all._

import cats.Show
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Json}

class Test extends testkit.Test {
  describe("circe") {
    it("decode json files") {
      case class Foo(foo: String, bar: Bar)
      case class Bar(bar: String)

      implicit val s = new Show[Foo] {
        override def show(f: Foo): String = ""
      }

      implicit val d = implicitly[Decoder[Foo]]
      val c = parse(Seq("--json", s"${getClass.getResource("/file.json").getFile}"))(
        arg[Foo]("json", Some("json file")))

      c.valid should === (Foo("foo", Bar("bar")))

      val c1 = parse(Seq("--json", s"${getClass.getResource("/file.json").getFile}"))(
        arg[Json]("json", Some("json file")))

      c1.valid should === (Foo("foo", Bar("bar")).asJson)
    }
  }
}
