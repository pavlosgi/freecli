package pavlosgi.freecli.argument.help

import cats.~>

import pavlosgi.freecli.argument.api._

object ArgumentHelpInterpreter extends (Algebra ~> Result) {
  def apply[A](fa: Algebra[A]): ArgumentsHelp = {
    fa match {
      case Arg(details, _, _) =>
        ArgumentsHelp(List(details))
    }
  }
}