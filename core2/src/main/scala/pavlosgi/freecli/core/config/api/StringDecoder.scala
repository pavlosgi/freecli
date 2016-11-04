package pavlosgi.freecli.core.api.config

import cats.data.{Validated, ValidatedNel}

trait StringDecoder[T] {
  def apply(field: Field, value: String): ValidatedNel[StringDecoderError, T]
}

object StringDecoder {
  implicit object stringStringDecoder extends StringDecoder[String] {
    override def apply(field: Field, value: String): ValidatedNel[StringDecoderError, String] =
      Validated.valid(value)
  }

  implicit object intStringDecoder extends StringDecoder[Int] {
    override def apply(field: Field, value: String): ValidatedNel[StringDecoderError, Int] =
      try {
        Validated.valid(value.toInt)
      } catch {
        case e: NumberFormatException =>
          Validated.invalidNel(StringDecoderError(field, s"Value $value is not a number"))

      }
  }

  implicit object booleanStringDecoder extends StringDecoder[Boolean] {
    override def apply(field: Field, value: String): ValidatedNel[StringDecoderError, Boolean] =
      try {
        Validated.valid(value.toBoolean)
      } catch {
        case e: IllegalArgumentException =>
          Validated.invalidNel(StringDecoderError(field, s"Value $value is not a boolean"))

      }
  }
}

case class StringDecoderError(field: Field, message: String)
