package pavlosgi.freecli.argument.interpreters

import cats.~>

import pavlosgi.freecli.argument.api._
import pavlosgi.freecli.core.{CommandLineArguments, ExtractSingle, ResultT, StringDecoder, StringDecoderError}

package object parser {
  type ParseResult[A] = ResultT[ArgumentParsingError, CommandLineArguments, A]

  implicit object argumentParserInterpreter extends (Algebra ~> ParseResult) {
    def apply[A](fa: Algebra[A]): ParseResult[A] = {
      fa match {
        case Arg(details, f, g) =>
          for {
            args   <- ResultT.get[ArgumentParsingError, CommandLineArguments]
            value  <- extractArgumentValue(details, args)
            res    <- parseArg(details, value, g)
          } yield f(res)
      }
    }
  }

  def parseArg[T](
    details: ArgumentDetails,
    value: String,
    g: StringDecoder[T]):
    ParseResult[T] = {

    ResultT.fromValidated[StringDecoderError, CommandLineArguments, T](
      g.apply(value)).leftMapInner[ArgumentParsingError] { e =>
        FailedToDecodeArgument(details, e)
      }
  }

  def extractArgumentValue(
    details: ArgumentDetails,
    args: CommandLineArguments):
    ParseResult[String] = {

    args.extractNext match {
      case ExtractSingle(_, None) =>
        ResultT.leftNE(ArgumentValueMissing(details))

      case ExtractSingle(u, Some(v)) =>
        ResultT.set(u).map(_ => v)
    }
  }
}