package pavlosgi.freecli.config.interpreters

import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.argument.interpreters.{help => A}
import pavlosgi.freecli.option.interpreters.{help => O}

package object help {
  type Res[A] = ConfigHelp
  implicit object configHelpInterpreter extends (Algebra ~> Res) {
    def apply[A](fa: Algebra[A]) = {
      fa match {
        case Opts(dsl) =>
          ConfigHelp.options(dsl.analyze(O.OptionHelpInterpreter))

        case Args(dsl) =>
          ConfigHelp.arguments(dsl.analyze(A.ArgumentsHelpInterpreter))

        case OptsAndArgs(opts, args, _) =>
          ConfigHelp.optionsAndArguments(
            opts.analyze(O.OptionHelpInterpreter),
            args.analyze(A.ArgumentsHelpInterpreter))
      }
    }
  }
}