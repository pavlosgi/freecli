package pavlosgi.freecli.core.interpreters.help

import cats.~>

import pavlosgi.freecli.core.api.arguments._
import pavlosgi.freecli.core.dsl.arguments.ArgumentsDsl

package object arguments {
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