package pavlosgi
package freecli

import cats.data.Validated

package object core {
  implicit class ValidatedOps[A, B](v: Validated[A, B]) {
    def invalid: A = v match {
      case Validated.Valid(_) => throw new IllegalArgumentException("Tried to access valid but was invalid")
      case Validated.Invalid(a) => a
    }

    def valid: B = v match {
      case Validated.Invalid(_) => throw new IllegalArgumentException("Tried to access valid but was invalid")
      case Validated.Valid(b) => b
    }
  }
}
