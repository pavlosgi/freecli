package pavlosgi
package freecli
package config
package circe

import all._
import parser.{GenParsingError, InvalidValueTypeGPE, ParserF}

import java.io.File
import scala.io.Source

import cats.data.Validated
import io.circe.{Decoder, Json, jawn}

trait Instances {
  implicit def parserInstance[T](implicit ev: Decoder[T]): ParserF[File, T] =
    new ParserF[File, T] {
      override def from(v: File): Validated[GenParsingError, T] =
        jawn.decode(Source.fromFile(v).mkString).toValidated
          .leftMap(e => InvalidValueTypeGPE(e.getMessage))
    }

  implicit def parserJsonInstance: ParserF[File, Json] = new ParserF[File, Json] {
    override def from(v: File): Validated[GenParsingError, Json] =
      jawn.parse(Source.fromFile(v).mkString).toValidated.leftMap(e =>
        InvalidValueTypeGPE(e.message))
  }
}
