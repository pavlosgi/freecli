package pavlosgi.freecli.config.parser

import cats.data._
import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.argument.{parser => A}
import pavlosgi.freecli.option.{parser => O}

object ConfigParserInterpreter extends (Algebra ~> ParseResult) {
  def apply[A](fa: Algebra[A]): ParseResult[A] = {
    fa match {
      case Args(args) =>
        A.ops.parseArgumentNonStrict(args)
          .mapError[ConfigParsingErrors](ers => NonEmptyList.of(ArgumentErrors(ers)))
          .mapAction[Action] { a => ArgumentAction(a) }

      case Opts(opts) =>
        O.ops.parseOptionNonStrict(opts)
          .mapError[ConfigParsingErrors](ers => NonEmptyList.of(OptionErrors(ers)))
          .mapAction[Action] { o => OptionAction(o) }

      case OptsAndArgs(opts, args, f) =>
        for {
          optsRes <- apply(Opts(opts))
          argsRes <- apply(Args(args))
        } yield f(optsRes, argsRes)
    }
  }
}