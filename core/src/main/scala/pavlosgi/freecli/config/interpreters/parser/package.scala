package pavlosgi.freecli.config.interpreters

import cats.data._
import cats.~>

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.argument.interpreters.{parser => A}
import pavlosgi.freecli.core.{Argument, Arguments, Marked, ResultT}
import pavlosgi.freecli.option.interpreters.{parser => O}

package object parser {
  type ParseResult[A] = ResultT[ConfigParsingError, Arguments, A]

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
            remainingArgs <- ResultT.get[ConfigParsingError, Arguments]
            newArgs = dropBeforeLastMarked(remainingArgs)
            _ <- ResultT.set(newArgs)
            argsRes <- apply(Args(args))
          } yield f(optsRes, argsRes)
      }
    }

    def dropBeforeLastMarked(args: Arguments): Arguments = {
      val newArgs =
        args.args.foldLeft(Seq.empty[Argument]) {
          case (r, Argument(_, Marked)) => Seq.empty
          case (r, a) => r :+ a
        }

      Arguments(newArgs)
    }
  }
}