package freecli
package config
package help

import cats.~>

import api._
import argument.{help => A}
import option.{help => O}

object ConfigHelpInterpreter extends (Algebra ~> HelpResult) {
  def apply[A](fa: Algebra[A]) = {
    fa match {
      case Opts(dsl) =>
        ConfigHelp.options(dsl.analyze(O.OptionHelpInterpreter))

      case Args(dsl) =>
        ConfigHelp.arguments(dsl.analyze(A.ArgumentHelpInterpreter))

      case OptsAndArgs(opts, args, _) =>
        ConfigHelp.optionsAndArguments(
          opts.analyze(O.OptionHelpInterpreter),
          args.analyze(A.ArgumentHelpInterpreter))
    }
  }
}