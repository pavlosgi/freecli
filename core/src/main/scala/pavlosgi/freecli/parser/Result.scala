package pavlosgi.freecli.parser

import cats.data.NonEmptyList

sealed trait Result[A, E, T]
case class Success[A, E, T](value: T) extends Result[A, E, T]
case class Failure[A, E, T](errors: NonEmptyList[E]) extends Result[A, E, T]
case class Action[A, E, T](action: A) extends Result[A, E, T]
