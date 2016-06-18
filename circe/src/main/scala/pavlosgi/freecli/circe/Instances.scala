package pavlosgi
package freecli
package circe

import java.io.File

import io.circe.{Decoder, Json, jawn}
import pavlosgi.freecli.core.algebra.ParserF

import scala.io.Source

trait Instances {
  implicit def parserInstance[T](implicit ev: Decoder[T]): ParserF[File, T] = new ParserF[File, T] {
    override def from(v: File): Option[T] = jawn.decode(Source.fromFile(v).mkString).toOption
  }

  implicit def parserJsonInstance: ParserF[File, Json] = new ParserF[File, Json] {
    override def from(v: File): Option[Json] = jawn.parse(Source.fromFile(v).mkString).toOption
  }
}
