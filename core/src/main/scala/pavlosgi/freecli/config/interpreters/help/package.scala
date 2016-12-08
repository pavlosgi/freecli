package pavlosgi.freecli.config.interpreters

import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.argument.interpreters.{help => A}
import pavlosgi.freecli.option.interpreters.{help => O}

package object help {
  type Result[A] = HelpState

  implicit object configHelpInterpreter extends (Algebra ~> Result) {
    def apply[A](fa: Algebra[A]): HelpState = {
      fa match {
        case Opts(dsl) =>
          HelpState(Some(dsl.analyze(O.optionHelpInterpreter)), None)

        case Args(dsl) =>
          HelpState(None, Some(dsl.analyze(A.argumentHelpInterpreter)))

        case OptsAndArgs(opts, args, _) =>
          HelpState(
            Some(opts.analyze(O.optionHelpInterpreter)),
            Some(args.analyze(A.argumentHelpInterpreter)))
      }
    }
  }
}