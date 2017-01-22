package pavlosgi.freecli.argument.parser

import cats.~>

import pavlosgi.freecli.argument.api._
import pavlosgi.freecli.core.api.StringDecoder
import pavlosgi.freecli.parser.CliParser

object ArgumentParserInterpreter extends (Algebra ~> ParseResult) {
  def apply[A](fa: Algebra[A]): ParseResult[A] = {
    fa match {
      case Arg(details, f, g) =>
        for {
          nextArg <- CliParser.extractNext[Action, ArgumentParsingError]
          res     <- parseArg(details, nextArg, g)
        } yield f(res)
    }
  }

  def parseArg[T](
    details: ArgumentField,
    value: Option[String],
    g: StringDecoder[T]):
    CliParser[Action, ArgumentParsingError, T] = {

    value match {
      case None =>
        CliParser.error(ArgumentValueMissing(details))

      case Some(v) =>
        CliParser.fromValidated(g.apply(v)).mapError { e =>
          FailedToDecodeArgument(details, e)
        }
    }
  }
}