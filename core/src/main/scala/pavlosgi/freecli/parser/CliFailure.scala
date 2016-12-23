package pavlosgi.freecli.parser

import cats.data.NonEmptyList
import cats.Semigroup
import cats.syntax.all._

case class CliFailure[E](value: Either[HelpRequested.type, NonEmptyList[E]])

object CliFailure {
  def help[E] = CliFailure[E](Left(HelpRequested))
  def errors[E](ers: NonEmptyList[E]) = CliFailure(Right(ers))

  implicit def semigroupInstance[E] = new Semigroup[CliFailure[E]] {
    def combine(x: CliFailure[E], y: CliFailure[E]): CliFailure[E] = {
      (x, y) match {
        case (CliFailure(Left(HelpRequested)), _) => CliFailure.help[E]
        case (_, CliFailure(Left(HelpRequested))) => CliFailure.help[E]
        case (CliFailure(Right(ers1)), CliFailure(Right(ers2))) =>
          CliFailure.errors(ers1.combine(ers2))
      }
    }
  }
}