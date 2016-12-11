package pavlosgi.freecli.option.interpreters

import cats.~>

import pavlosgi.freecli.option.api._

package object help {
  type Result[A] = HelpState

  implicit object optionHelpInterpreter extends (Algebra ~> Result) {
    def apply[A](fa: Algebra[A]): HelpState = {
      fa match {
        case RequiredOpt(field, _, g, default) =>
          HelpState(List(
            OptionFieldHelp(field, required = true, default.map(g.toString))))

        case Opt(field, _, _) =>
          HelpState(List(OptionFieldHelp(field, required = false)))

        case Flag(field, _) =>
          HelpState(List(OptionFieldHelp(field, required = false)))

        case Sub(description, dsl) =>
          HelpState(List(SubHelp(description, dsl.analyze(optionHelpInterpreter))))

      }
    }
  }
}