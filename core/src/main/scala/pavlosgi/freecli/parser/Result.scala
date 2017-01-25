package pavlosgi.freecli.parser

import cats.Semigroup

sealed abstract class Result[A, E: Semigroup, T]
case class Success[A, E: Semigroup, T](value: T) extends Result[A, E, T]
case class Failure[A, E: Semigroup, T](error: E) extends Result[A, E, T]
case class Action[A, E: Semigroup, T](action: A) extends Result[A, E, T]
