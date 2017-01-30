package freecli
package option
package help

import cats.~>

import option.api._

object OptionHelpInterpreter extends (Algebra ~> HelpResult) {
  def apply[A](fa: Algebra[A]): HelpResult[A] = {
    fa match {
      case RequiredOpt(field, _, g, default) =>
        OptionsHelp.single(SingleOptionHelp(field, default.map(g.toString), true))

      case Opt(field, _, _) =>
        OptionsHelp.single(SingleOptionHelp(field, None, false))

      case Flag(field, _) =>
        OptionsHelp.single(SingleOptionHelp(field, None, false))

      case Help(field, _) =>
        OptionsHelp.single(SingleOptionHelp(field, None, false))

      case Version(field, _, _) =>
        OptionsHelp.single(SingleOptionHelp(field, None, false))

      case Sub(description, dsl) =>
        OptionsHelp.single(SubOptionHelp(description.value, dsl.analyze(OptionHelpInterpreter)))
    }
  }
}