package pavlosgi.freecli.argument.interpreters

import cats.~>

import pavlosgi.freecli.argument.api._

package object help {
  type Result[A] = ArgumentsHelp
  object ArgumentsHelpInterpreter extends (Algebra ~> Result) {
    def apply[A](fa: Algebra[A]): ArgumentsHelp = {
      fa match {
        case Arg(details, _, _) =>
          ArgumentsHelp(List(details))
      }
    }
  }
}