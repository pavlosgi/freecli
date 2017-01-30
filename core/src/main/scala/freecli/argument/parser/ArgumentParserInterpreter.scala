package freecli
package argument
package parser

import cats.data.NonEmptyList
import cats.~>

import api._
import core.api.StringDecoder
import freecli.parser.CliParser

object ArgumentParserInterpreter extends (Algebra ~> ParseResult) {
  def apply[A](fa: Algebra[A]): ParseResult[A] = {
    fa match {
      case Arg(details, f, g) =>
        for {
          nextArg <- CliParser.extractNext[Action, ArgumentParsingErrors]
          res     <- parseArg(details, nextArg, g)
        } yield f(res)
    }
  }

  def parseArg[T](
    details: ArgumentField,
    value: Option[String],
    g: StringDecoder[T]):
    CliParser[Action, ArgumentParsingErrors, T] = {

    value match {
      case None =>
        CliParser.error(NonEmptyList.of(ArgumentValueMissing(details)))

      case Some(v) =>
        CliParser.fromValidated(g.apply(v)).mapError { e =>
          e.map(err => FailedToDecodeArgument(details, err))
        }
    }
  }
}