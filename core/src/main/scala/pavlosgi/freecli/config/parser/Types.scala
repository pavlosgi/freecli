package pavlosgi
package freecli
package config
package parser

import algebra.{Field, SubField}

import cats.Show
import cats.data.{Validated, ValidatedNel}
import cats.syntax.all._

trait Types {
  type Parser[T] = parser.Parser[T]

  type ParsingError = parser.ParsingError
  val ParsingError = parser.ParsingError
  type FieldMissing = parser.FieldMissing
  val FieldMissing = parser.FieldMissing
  type FieldValueMissing = parser.FieldValueMissing
  val FieldValueMissing = parser.FieldValueMissing
  type InvalidValueType = parser.InvalidValueType
  val InvalidValueType = parser.InvalidValueType
  type InvalidArgs = parser.InvalidArgs
  val InvalidArgs = parser.InvalidArgs
  type SubFieldMissing = parser.SubFieldMissing
  val SubFieldMissing = parser.SubFieldMissing
}

trait Parser[T] {
  def apply(v: String): ValidatedNel[GenParsingError, T]
}

abstract class ParserF[F, T](implicit ev: Parser[F]) extends Parser[T] {
  def from(v: F): ValidatedNel[GenParsingError, T]
  def apply(v: String): ValidatedNel[GenParsingError, T] = ev(v) match {
    case Validated.Valid(a) => from(a)
    case inv@Validated.Invalid(_) => inv
  }
}

sealed trait GenParsingError
object GenParsingError {
  def toParsingError(field: Field, g: GenParsingError): ParsingError = {
    g match {
      case InvalidValueTypeGPE(error) =>
        InvalidValueType(field, error)
    }
  }
}

case class InvalidValueTypeGPE(error: String) extends GenParsingError

sealed trait ParsingError

object ParsingError {
  implicit object showInstance extends Show[ParsingError] {
    override def show(f: ParsingError): String = {
      f match {
        case FieldMissing(field) =>
          s"${field.name.show} is missing"

        case FieldValueMissing(field) =>
          s"${field.name.show} value is missing"

        case InvalidValueType(field, value) =>
          s"Field ${field.name.show} value $value is invalid"

        case InvalidArgs(args) =>
        s"Args ${args.mkString(",")} are invalid"

        case SubFieldMissing(field) =>
          s"${field.name.show} is missing"

      }
    }
  }
}

case class FieldMissing(field: Field) extends ParsingError
case class FieldValueMissing(field: Field) extends ParsingError
case class InvalidValueType(field: Field, value: String) extends ParsingError
case class InvalidArgs(args: Seq[String]) extends ParsingError
case class SubFieldMissing(field: SubField) extends ParsingError
