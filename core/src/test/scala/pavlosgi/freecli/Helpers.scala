package pavlosgi.freecli

import cats.Semigroup

import pavlosgi.freecli.parser._

object Helpers {
  implicit class CliFailureOps[A, E: Semigroup](cliFailure: EarlyTermination[A, E]) {
    def errors = cliFailure match {
      case ActionTermination(a) =>
        throw new IllegalArgumentException(s"Tried to access errors in failure but failure was Action $a")

      case ErrorTermination(errors) => errors
    }
  }

  implicit class ResultOps[A, E: Semigroup, T](result: Result[A, E, T]) {
    def success = result match {
      case Success(a) => a
      case a =>
        throw new IllegalArgumentException(s"Tried to access Success but was $a")
    }

    def failure = result match {
      case Failure(e) => e
      case a =>
        throw new IllegalArgumentException(s"Tried to access Failure but was $a")
    }

    def action = result match {
      case Action(a) => a
      case a =>
        throw new IllegalArgumentException(s"Tried to access Action but was $a")
    }
  }
}
