package pavlosgi.freecli.config.help

import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.argument.{help => A}
import pavlosgi.freecli.option.{help => O}

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