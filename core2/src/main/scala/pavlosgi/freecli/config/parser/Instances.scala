package pavlosgi
package freecli
package config
package parser

import java.io.File

import cats.data.{Validated, ValidatedNel}

trait Instances {
  implicit def strParser = new Parser[String] {
    override def apply(v: String): ValidatedNel[GenParsingError, String] =
      Validated.Valid(v)
  }

  implicit def boolParser = new Parser[Boolean] {
    override def apply(v: String): ValidatedNel[GenParsingError, Boolean] = {
      try {
        Validated.Valid(v.toBoolean)
      } catch {
        case e: IllegalArgumentException =>
          Validated.invalidNel(InvalidValueTypeGPE(v))
      }
    }
  }

  implicit def intParser = new Parser[Int] {
    override def apply(v: String): ValidatedNel[GenParsingError, Int] = {
      try {
        Validated.Valid(v.toInt)
      } catch {
        case e: NumberFormatException =>
          Validated.invalidNel(InvalidValueTypeGPE(v))
      }
    }
  }

  implicit def fileParser = new Parser[File] {
    override def apply(v: String): ValidatedNel[GenParsingError, File] =
      Validated.Valid(new File(v))
  }
}