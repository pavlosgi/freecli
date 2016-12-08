package pavlosgi.freecli.config.interpreters

import cats.data._
import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.argument.interpreters.{parser => A}
import pavlosgi.freecli.core.Arguments
import pavlosgi.freecli.core.ResultTS
import pavlosgi.freecli.option.interpreters.{parser => O}

package object parser {
  type ResultT[A] = ResultTS[ConfigParsingError, Arguments, A]

  implicit object configParserInterpreter extends (Algebra ~> ResultT) {
    def apply[A](fa: Algebra[A]): ResultT[A] = {
      fa match {
        case Args(args) =>
          args.foldMap(A.argumentParserInterpreter)
            .leftMap(ers => NonEmptyList.of(ArgumentErrors(ers)))

        case Opts(opts) =>
          opts.foldMap(O.optionParserInterpreter)
            .leftMap(ers => NonEmptyList.of(OptionErrors(ers)))

        case OptsAndArgs(opts, args, f) =>
          for {
            optsRes <- apply(Opts(opts))
            argsRes <- apply(Args(args))
          } yield f(optsRes, argsRes)
      }
    }
  }
}