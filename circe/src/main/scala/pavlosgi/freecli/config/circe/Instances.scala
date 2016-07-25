package pavlosgi
package freecli
package config
package circe

import all._
import parser.{GenParsingError, InvalidValueTypeGPE, ParserF}
import java.io.File
import scala.io.Source

import cats.data.ValidatedNel
import cats.std.all._
import io.circe.{Decoder, Json, jawn, Error}

trait Instances {
  implicit def parserInstance[T](implicit ev: Decoder[T]): ParserF[File, T] =
    new ParserF[File, T] {
      override def from(v: File): ValidatedNel[GenParsingError, T] =
        jawn.decode(Source.fromFile(v).mkString).toValidatedNel[Error]
          .leftMap(e => e.map[GenParsingError](err =>
            InvalidValueTypeGPE(err.getMessage)))
    }

  implicit def parserJsonInstance: ParserF[File, Json] = new ParserF[File, Json] {
    override def from(v: File): ValidatedNel[GenParsingError, Json] =
      jawn.parse(Source.fromFile(v).mkString).toValidatedNel[Error]
        .leftMap(e => e.map[GenParsingError](err =>
            InvalidValueTypeGPE(err.getMessage)))
  }
}
