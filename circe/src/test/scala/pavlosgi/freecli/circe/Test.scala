package pavlosgi.freecli.circe

import scala.io.Source

import cats.Show
import io.circe.generic.auto._
import io.circe.Json
import io.circe.parser._

import pavlosgi.freecli.core.api.StringDecoder
import pavlosgi.freecli.circe.all._
import pavlosgi.freecli.testkit

class Test extends testkit.Test {
  describe("Circe Json StringDecoder") {

    it("decode inline string to json") {
      implicitly[StringDecoder[Json]].apply("""{"test": "test"}""").valid should === (
        parse("""{"test": "test"}""").toOption.get)
    }

    it("decode inline to case class via json") {
      case class Foo(test: String)

      implicit val s = new Show[Foo] {
        override def show(f: Foo): String = f.test
      }

      implicitly[StringDecoder[Foo]].apply("""{"test": "test"}""").valid should === (
        Foo("test"))
    }

    it("decode file to json") {
      val file = getClass.getResource("/test.json").getFile
      val fileContents = Source.fromFile(file).mkString

      implicitly[StringDecoder[Json]].apply(file).valid should
        === (parse(fileContents).toOption.get)
    }

    it("decode file to case class via json") {
      val file = getClass.getResource("/test.json").getFile
      val fileContents = Source.fromFile(file).mkString

      case class Foo(foo: String, bar: Bar)
      case class Bar(bar: String)

      implicit val s = new Show[Foo] {
        override def show(f: Foo): String = f.toString
      }

      implicitly[StringDecoder[Foo]].apply(file).valid should === (
        decode[Foo](fileContents).toOption.get)
    }
  }
}
