package pavlosgi.freecli.core

import java.io.File

import cats.data.{Validated, ValidatedNel}

trait StringDecoder[T] {
  type Out = T
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
          Validated.invalidNel(StringDecoderError(s"Value $value is not an Int"))

      }
    }

    def toString(v: Int): String = v.toString
  }

  implicit object doubleStringDecoder extends StringDecoder[Double] {
    override def apply(
      value: String):
      ValidatedNel[StringDecoderError, Double] = {
      try {
        Validated.valid(value.toDouble)
      } catch {
        case e: NumberFormatException =>
          Validated.invalidNel(StringDecoderError(s"Value $value is not a Double"))

      }
    }

    def toString(v: Double): String = v.toString
  }

  implicit object longStringDecoder extends StringDecoder[Long] {
    override def apply(
      value: String):
      ValidatedNel[StringDecoderError, Long] = {
      try {
        Validated.valid(value.toLong)
      } catch {
        case e: NumberFormatException =>
          Validated.invalidNel(StringDecoderError(s"Value $value is not a Long"))

      }
    }

    def toString(v: Long): String = v.toString
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

  implicit object newFileStringDecoder extends StringDecoder[NewFile] {
    override def apply(
      value: String):
      ValidatedNel[StringDecoderError, NewFile] = {

      val file = new File(value)
      if (file.exists) {
        Validated.invalidNel(
          StringDecoderError(s"File ${file.getAbsolutePath} already exists"))

      } else {
        Validated.Valid(NewFile(file))
      }
    }

    def toString(v: NewFile): String = v.getAbsolutePath
  }

  implicit object existentFileStringDecoder extends StringDecoder[ExistentFile] {
    override def apply(
      value: String):
      ValidatedNel[StringDecoderError, ExistentFile] = {

      val file = new File(value)
      if (file.exists) {
        Validated.Valid(ExistentFile(file))
      } else {
        Validated.invalidNel(
          StringDecoderError(s"File ${file.getAbsolutePath} does not exist"))
      }
    }

    def toString(v: ExistentFile): String = v.getAbsolutePath
  }

  implicit object fileStringDecoder extends StringDecoder[File] {
    override def apply(
      value: String):
      ValidatedNel[StringDecoderError, File] = {

      Validated.Valid(new File(value))
    }

    def toString(v: File): String = v.getAbsolutePath
  }
}

case class StringDecoderError(message: String)
