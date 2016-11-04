package pavlosgi.freecli.core

import cats.data.{EitherT, _}

package object interpreters {
  type ResultT[E, A] = EitherT[State[Arguments, ?], NonEmptyList[E], A]
  type ResultTS[E, S, A] = EitherT[State[S, ?], NonEmptyList[E], A]
  type Result[_] = State[HelpState, Unit]
}
