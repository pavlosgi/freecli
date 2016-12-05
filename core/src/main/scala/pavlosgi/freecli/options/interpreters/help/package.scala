package pavlosgi.freecli.options.interpreters

import cats.~>

import pavlosgi.freecli.core._
import pavlosgi.freecli.options.api._
import pavlosgi.freecli.options.dsl.OptionsDsl

package object help {
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