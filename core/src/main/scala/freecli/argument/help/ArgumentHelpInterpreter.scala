package freecli
package argument
package help

import cats.~>

import api._

object ArgumentHelpInterpreter extends (Algebra ~> Result) {
  def apply[A](fa: Algebra[A]): ArgumentsHelp = {
    fa match {
      case Arg(details, _, _) =>
        ArgumentsHelp(List(details))
    }
  }
}