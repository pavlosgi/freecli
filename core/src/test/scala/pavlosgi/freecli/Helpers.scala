package pavlosgi
package freecli

import cats.data.Validated

trait Helpers {
  implicit class ValidatedOps[A, B](v: Validated[A, B]) {
    def invalid: A = v match {
      case valid@Validated.Valid(_) =>
        throw new IllegalArgumentException(
          s"Tried to access invalid but was valid $valid")

      case Validated.Invalid(a) => a
    }

    def valid: B = v match {
      case invalid@Validated.Invalid(_) =>
        throw new IllegalArgumentException(
          s"Tried to access valid but was invalid $invalid")

      case Validated.Valid(b) => b
    }
  }
}
