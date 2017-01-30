package freecli
package circe

import scala.io.Source

import cats.Show
import cats.syntax.show._
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import io.circe.{Decoder, Json}
import io.circe.parser.parse

import core.api.{StringDecoder, StringDecoderError}

trait Instances {
  implicit def jsonDecodedStringDecoder[T](
    implicit ev: StringDecoder[Json],
    show: Show[T],
    decoder: Decoder[T]):
    StringDecoder[T] = {

    new StringDecoder[T] {
      def apply(value: String): ValidatedNel[StringDecoderError, T] = {
        ev(value) match {
          case Validated.Valid(j) =>
            Validated.fromEither(j.as[T]).leftMap(
              f => NonEmptyList.of(StringDecoderError(f.message)))

          case Validated.Invalid(e) => Validated.Invalid(e)
        }
      }

      def toString(v: T): String = {
        v.show
      }
    }
  }

  implicit def jsonStringDecoder: StringDecoder[Json] = {
    new StringDecoder[Json] {
      def apply(value: String): ValidatedNel[StringDecoderError, Json] = {

        val stringToParse =
          if (value.matches(".+\\.json")) {
            Source.fromFile(value).mkString
          } else value

        parse(stringToParse) match {
          case Right(j) =>
            Validated.valid(j)

          case Left(e) => Validated.invalidNel(StringDecoderError(e.message))
        }
      }

      def toString(v: Json): String = {
        v.spaces2
      }
    }
  }
}

