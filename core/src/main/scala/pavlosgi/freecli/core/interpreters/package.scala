package pavlosgi.freecli.core

import cats.data.{EitherT, _}

package object interpreters {
  private[interpreters] type ResultTS[E, S, A] = EitherT[State[S, ?], NonEmptyList[E], A]
}
