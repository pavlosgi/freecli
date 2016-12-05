package pavlosgi.freecli.core.api

import cats.data.{Validated, ValidatedNel}

trait StringDecoder[T] {
  def apply(value: String): ValidatedNel[StringDecoderError, T]
  def toString(v: T): String
}

object StringDecoder {
  implicit object stringStringDecoder extends StringDecoder[String] {
    override def apply(
      value: String):
      ValidatedNel[StringDecoderError, String] = {

      Validated.valid(value)
    }

    def toString(v: String): String = v
  }

  implicit object intStringDecoder extends StringDecoder[Int] {
    override def apply(
      value: String):
      ValidatedNel[StringDecoderError, Int] = {
      try {
        Validated.valid(value.toInt)
      } catch {
        case e: NumberFormatException =>
          Validated.invalidNel(StringDecoderError(s"Value $value is not a number"))

      }
    }

    def toString(v: Int): String = v.toString
  }

  implicit object booleanStringDecoder extends StringDecoder[Boolean] {
    override def apply(
      value: String):
      ValidatedNel[StringDecoderError, Boolean] = {

      try {
        Validated.valid(value.toBoolean)
      } catch {
        case e: IllegalArgumentException =>
          Validated.invalidNel(StringDecoderError(s"Value $value is not a boolean"))

      }
    }

    def toString(v: Boolean): String = v.toString
  }
}

case class StringDecoderError(message: String)
