package pavlosgi.freecli.argument.interpreters

import cats.~>

import pavlosgi.freecli.argument.api._

package object help {
  type Result[A] = HelpState

  implicit object argumentHelpInterpreter extends (Algebra ~> Result) {
    def apply[A](fa: Algebra[A]): HelpState = {
      fa match {
        case Arg(details, _, _) =>
          HelpState(List(details))
      }
    }
  }
}