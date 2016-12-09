package pavlosgi.freecli.argument.interpreters

import cats.~>

import pavlosgi.freecli.argument.api._
import pavlosgi.freecli.core.{Arguments, ResultT, StringDecoder, StringDecoderError}

package object parser {
  type ParseResult[A] = ResultT[ArgumentParsingError, Arguments, A]

  implicit object argumentParserInterpreter extends (Algebra ~> ParseResult) {
    def apply[A](fa: Algebra[A]): ParseResult[A] = {
      fa match {
        case Arg(details, f, g) =>
          for {
            args   <- ResultT.get[ArgumentParsingError, Arguments]
            value  <- extractArgumentValue(details, args)
            res    <- parseArg(details, value, g)
          } yield f(res)
      }
    }
  }

  def parseArg[T](details: ArgumentDetails, value: String, g: StringDecoder[T]) = {
    ResultT.fromValidated[StringDecoderError, Arguments, T](
      g.apply(value)).leftMapInner[ArgumentParsingError] { e =>
        FailedToDecodeArgument(details, e)
      }
  }

  def extractArgumentValue(
    details: ArgumentDetails,
    args: Arguments):
    ParseResult[String] = {

    args.args.headOption match {
      case None =>
        ResultT.leftNE(ArgumentValueMissing(details))

      case Some(v) =>
        val remArgs = args.args.drop(1)
          ResultT.set(Arguments(remArgs)).map(_ => v)
    }
  }
}