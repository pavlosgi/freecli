package pavlosgi.freecli.core

import java.io.File

import cats.data.{Validated, ValidatedNel}
import cats.instances.all._
import cats.syntax.all._

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

  implicit def seqDecoder[T](implicit ev: StringDecoder[List[T]]) = new StringDecoder[Seq[T]] {
    def apply(value: String): ValidatedNel[StringDecoderError, Seq[T]] = {
      ev.apply(value).map(_.toSeq)
    }

    def toString(v: Seq[T]): String = ev.toString(v.toList)
  }

  implicit def listDecoder[T](implicit ev: StringDecoder[T]) = new StringDecoder[List[T]] {
    def apply(value: String): ValidatedNel[StringDecoderError, List[T]] = {
      value.split(",").toList.traverseU(ev.apply)
    }

    def toString(v: List[T]): String = v.map(ev.toString).mkString(",")
  }

  implicit def mapDecoder[K, V](implicit ev: StringDecoder[K], ev2: StringDecoder[V]) =
    new StringDecoder[Map[K, V]] {
      def apply(value: String): ValidatedNel[StringDecoderError, Map[K, V]] = {
        value.split(",").map(_.split("=", 2)).toList.traverseU {
          case Array(k, v) =>
            (ev(k) |@| ev2(v)).map(_ -> _)

          case arr =>
            Validated.invalidNel(
              StringDecoderError(s"Invalid key value pair ${arr.mkString}"))

        }.map(_.toMap)
      }

      def toString(v: Map[K, V]): String = v.toList.map {
        case (k, v) => s"${ev.toString(k)}=${ev2.toString(v)}"
      }.mkString(",")
    }
}

case class StringDecoderError(message: String)
