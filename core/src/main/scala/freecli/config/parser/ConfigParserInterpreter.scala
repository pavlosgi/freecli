package freecli
package config
package parser

import cats.data._
import cats.~>

import api._
import argument.{parser => A}
import option.{parser => O}

class ConfigParserInterpreter(optsLookAhead: Boolean) extends (Algebra ~> ParseResult) {
  def apply[A](fa: Algebra[A]): ParseResult[A] = {
    fa match {
      case Args(args) =>
        A.ops.parseArgumentNonStrict(args)
          .mapError[ConfigParsingErrors](ers => NonEmptyList.of(ArgumentErrors(ers)))
          .mapAction[Action] { a => ArgumentAction(a) }

      case Opts(opts) =>
        O.ops.parseOptionNonStrict(opts, optsLookAhead)
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
