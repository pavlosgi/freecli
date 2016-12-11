package pavlosgi.freecli.config.interpreters

import cats.data._
import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.argument.interpreters.{parser => A}
import pavlosgi.freecli.core.{CommandLineArguments, ResultT}
import pavlosgi.freecli.option.interpreters.{parser => O}

package object parser {
  type ParseResult[A] = ResultT[ConfigParsingError, CommandLineArguments, A]

  implicit object configParserInterpreter extends (Algebra ~> ParseResult) {
    def apply[A](fa: Algebra[A]): ParseResult[A] = {
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
            cliArgs <- ResultT.get[ConfigParsingError, CommandLineArguments]
            _ <- ResultT.set(cliArgs.markAllBeforeLastMarked)
            argsRes <- apply(Args(args))
          } yield f(optsRes, argsRes)
      }
    }
  }
}