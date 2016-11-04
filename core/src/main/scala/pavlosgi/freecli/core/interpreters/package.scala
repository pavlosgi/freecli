package pavlosgi.freecli.core

import cats.data.{XorT, _}

package object interpreters {
  type ResultT[E, A] = XorT[State[Arguments, ?], NonEmptyList[E], A]
  type ResultTS[E, S, A] = XorT[State[S, ?], NonEmptyList[E], A]
  type Result[_] = State[HelpState, Unit]
}
