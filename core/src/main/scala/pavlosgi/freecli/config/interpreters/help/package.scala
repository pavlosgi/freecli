package pavlosgi.freecli.config.interpreters

import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.config.dsl.ConfigDsl
import pavlosgi.freecli.arguments.interpreters.{help => A}
import pavlosgi.freecli.core._
import pavlosgi.freecli.options.interpreters.{help => O}

package object help {
  type Result[A] = HelpState

  def configHelp[A](dsl: ConfigDsl[A]): String = {
    val result = dsl.analyze(configAlgebraHelp)
    val argsOneLine = result.arguments.map(A.HelpState.oneline)

    s"""
       |${"Usage".bold.underline}
       |
       |  Program [options] ${argsOneLine.getOrElse("")}
       |
       |${HelpState.display(4, result)}
       |
       |""".stripMargin
  }

  implicit object configAlgebraHelp extends (Algebra ~> Result) {
    def apply[A](fa: Algebra[A]): HelpState = {
      fa match {
        case Opts(dsl) =>
          HelpState(Some(dsl.analyze(O.optionsAlgebraHelp)), None)

        case Args(dsl) =>
          HelpState(None, Some(dsl.analyze(A.argumentsAlgebraHelp)))

        case OptsAndArgs(opts, args, _) =>
          HelpState(
            Some(opts.analyze(O.optionsAlgebraHelp)),
            Some(args.analyze(A.argumentsAlgebraHelp)))
      }
    }
  }
}