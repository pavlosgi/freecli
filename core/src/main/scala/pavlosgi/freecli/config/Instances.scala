package pavlosgi
package freecli
package config

import algebra.Plugin
import parser.Parser

import cats._

trait Instances
  extends dsl.Instances
  with help.Instances
  with parser.Instances {

  sealed trait ParserShow[T] {
    def show: Show[T]
    def parser: Parser[T]
  }

  implicit object parserShowPlugin extends Plugin[ParserShow] {}

  implicit def fromShowAndParser[T]
    (implicit ev: Show[T], ev2: Parser[T]): ParserShow[T] = {

    new ParserShow[T] {
      override def show: Show[T] = ev
      override def parser: Parser[T] = ev2
    }
  }

  implicit object showNat extends (ParserShow ~> Show) {
    override def apply[A](fa: ParserShow[A]): Show[A] = fa.show
  }

  implicit object parserNat extends (ParserShow ~> Parser) {
    override def apply[A](fa: ParserShow[A]): Parser[A] = fa.parser
  }

}

object Instances extends Instances