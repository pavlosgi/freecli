package pavlosgi.freecli.arguments.interpreters

import cats.~>

import pavlosgi.freecli.arguments.api._
import pavlosgi.freecli.arguments.dsl.ArgumentsDsl
import pavlosgi.freecli.core._

package object help {
  type Result[A] = HelpState

  def argumentsHelp[A](dsl: ArgumentsDsl[A]): String = {
    val result = dsl.analyze(argumentsAlgebraHelp)
    val argsOneLine = HelpState.oneline(result)

    s"""
       |${"Usage".bold.underline}
       |
       |  Program $argsOneLine
       |
       |${HelpState.display(4, result)}
       |
       |""".stripMargin
  }

  implicit object argumentsAlgebraHelp extends (Algebra ~> Result) {
    def apply[A](fa: Algebra[A]): HelpState = {
      fa match {
        case Arg(details, _, _) =>
          HelpState(List(details))
      }
    }
  }
}