package pavlosgi.freecli.circe

import scala.io.Source

import cats.Show
import io.circe.generic.auto._
import io.circe.{Decoder, Json}
import io.circe.syntax._

import pavlosgi.freecli.config._
import pavlosgi.freecli.testkit

class Test extends testkit.Test {
  describe("circe") {
    it("decode json files") {
      case class Foo(foo: String, bar: Bar)
      case class Bar(bar: String)

      implicit val s = new Show[Foo] {
        override def show(f: Foo): String = ""
      }

      val inlineJson =
        Source.fromFile(getClass.getResource("/file.json").getFile).mkString

      implicit val d = implicitly[Decoder[Foo]]
      val res = parseConfig(Seq(s"${getClass.getResource("/file.json").getFile}"))(
        arg[Foo])

      res.valid should === (Foo("foo", Bar("bar")))

      val res2 = parseConfig(Seq(inlineJson))(arg[Foo])
      res2.valid should === (Foo("foo", Bar("bar")))

      val res3 = parseConfig(Seq(s"${getClass.getResource("/file.json").getFile}"))(
        arg[Json])

      res3.valid should === (Foo("foo", Bar("bar")).asJson)
    }
  }
}
