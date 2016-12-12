package pavlosgi.freecli.option.interpreters

import cats.~>

import pavlosgi.freecli.option.api._

package object help {
  type Result[A] = OptionsHelp

  implicit object OptionHelpInterpreter extends (Algebra ~> Result) {
    def apply[A](fa: Algebra[A]): Result[A] = {
      fa match {
        case RequiredOpt(field, _, g, default) =>
          OptionsHelp.single(SingleOptionHelp(field, default.map(g.toString), true))

        case Opt(field, _, _) =>
          OptionsHelp.single(SingleOptionHelp(field, None, false))

        case Flag(field, _) =>
          OptionsHelp.single(SingleOptionHelp(field, None, false))

        case Sub(description, dsl) =>
          OptionsHelp.single(SubOptionHelp(description.value, dsl.analyze(OptionHelpInterpreter)))
      }
    }
  }
}