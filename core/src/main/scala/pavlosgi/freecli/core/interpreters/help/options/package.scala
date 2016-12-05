package pavlosgi.freecli.core.interpreters.help

import cats.~>

import pavlosgi.freecli.core.api.options._
import pavlosgi.freecli.core.dsl.options.OptionsDsl

package object options {
  type Result[A] = HelpState

  def optionsHelp[A](dsl: OptionsDsl[A]): String = {
    val result = dsl.analyze(optionsAlgebraHelp)

    s"""
       |${"Usage".bold.underline}
       |
       |  Program [options]
       |
       |${HelpState.display(4, result)}
       |
       |""".stripMargin
  }

  implicit object optionsAlgebraHelp extends (Algebra ~> Result) {
    def apply[A](fa: Algebra[A]): HelpState = {
      fa match {
        case RequiredOpt(field, _, g, default) =>
          HelpState(List(OptionFieldHelp(field, default.map(g.toString))))

        case Opt(field, _, _) =>
          HelpState(List(OptionFieldHelp(field)))

        case Flag(field, _) =>
          HelpState(List(OptionFieldHelp(field)))

        case Sub(description, dsl) =>
          HelpState(List(SubHelp(description, dsl.analyze(optionsAlgebraHelp))))
      }
    }
  }
}