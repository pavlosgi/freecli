package pavlosgi.freecli.config.parser

import cats.data._
import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.argument.{parser => A}
import pavlosgi.freecli.option.{parser => O}
import pavlosgi.freecli.parser.CliParser

object ConfigParserInterpreter extends (Algebra ~> ParseResult) {
  def apply[A](fa: Algebra[A]): ParseResult[A] = {
    fa match {
      case Args(args) =>
        args.foldMap(A.ArgumentParserInterpreter)
          .leftMap(ers => NonEmptyList.of(ArgumentErrors(ers)))

      case Opts(opts) =>
        opts.foldMap(O.OptionParserInterpreter)
          .leftMap(ers => NonEmptyList.of(OptionErrors(ers)))

      case OptsAndArgs(opts, args, f) =>
        for {
          optsRes <- apply(Opts(opts))
          _ <- CliParser.markUnusableBeforeLastUsed[ConfigParsingError]
          argsRes <- apply(Args(args))
        } yield f(optsRes, argsRes)
    }
  }
}