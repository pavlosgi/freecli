package pavlosgi.freecli.argument.interpreters

import cats.~>

import pavlosgi.freecli.argument.api._
import pavlosgi.freecli.core.StringDecoder
import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[ArgumentParsingError, A]

  implicit object argumentParserInterpreter extends (Algebra ~> ParseResult) {
    def apply[A](fa: Algebra[A]): ParseResult[A] = {
      fa match {
        case Arg(details, f, g) =>
          for {
            nextArg <- CliParser.extractNext[ArgumentParsingError]
            res     <- parseArg(details, nextArg, g)
          } yield f(res)
      }
    }
  }

  def parseArg[T](
    details: ArgumentField,
    value: Option[String],
    g: StringDecoder[T]):
    CliParser[ArgumentParsingError, T] = {

    value match {
      case None =>
        CliParser.failed(ArgumentValueMissing(details))

      case Some(v) =>
        CliParser.fromValidated(g.apply(v)).leftMapInner { e =>
          FailedToDecodeArgument(details, e)
        }
    }
  }
}