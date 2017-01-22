package pavlosgi.freecli.parser

import cats.data.NonEmptyList
import cats.Semigroup
import cats.syntax.all._

sealed trait EarlyTermination[A, E]
case class ActionTermination[A, E](action: A) extends EarlyTermination[A, E]
case class ErrorTermination[A, E](errors: NonEmptyList[E]) extends EarlyTermination[A, E]

object EarlyTermination {
  implicit def semigroupInstance[A, E] = new Semigroup[EarlyTermination[A, E]] {
    def combine(x: EarlyTermination[A, E], y: EarlyTermination[A, E]): EarlyTermination[A, E] = {
      (x, y) match {
        case (ActionTermination(a), _) => ActionTermination(a)
        case (_, ActionTermination(a)) => ActionTermination(a)
        case (ErrorTermination(ers1), ErrorTermination(ers2)) =>
          ErrorTermination(ers1.combine(ers2))
      }
    }
  }
}