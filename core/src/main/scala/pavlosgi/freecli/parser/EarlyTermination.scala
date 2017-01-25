package pavlosgi.freecli.parser

import cats.Semigroup
import cats.syntax.all._

sealed abstract class EarlyTermination[A, E: Semigroup]
case class ActionTermination[A, E: Semigroup](action: A) extends EarlyTermination[A, E]
case class ErrorTermination[A, E: Semigroup](errors: E) extends EarlyTermination[A, E]

object EarlyTermination {
  implicit def semigroupInstance[A, E: Semigroup] = new Semigroup[EarlyTermination[A, E]] {
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