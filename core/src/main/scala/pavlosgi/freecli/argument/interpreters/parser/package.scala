package pavlosgi.freecli.argument.interpreters

import cats.~>

import pavlosgi.freecli.argument.api._
import pavlosgi.freecli.core.{Arguments, ResultTS, StringDecoder, StringDecoderError}

package object parser {
  type ResultT[A] = ResultTS[ArgumentParsingError, Arguments, A]

  implicit object argumentParserInterpreter extends (Algebra ~> ResultT) {
    def apply[A](fa: Algebra[A]): ResultT[A] = {
      fa match {
        case Arg(details, f, g) =>
          for {
            args   <- ResultTS.get[ArgumentParsingError, Arguments]
            value  <- extractArgumentValue(details, args)
            res    <- parseArg(details, value, g)
          } yield f(res)
      }
    }
  }

  def parseArg[T](details: ArgumentDetails, value: String, g: StringDecoder[T]) = {
    ResultTS.fromValidated[StringDecoderError, Arguments, T](
      g.apply(value)).leftMap { e =>
        e.map[ArgumentParsingError](er =>
          FailedToDecodeArgument(details, er))
      }
  }

  def extractArgumentValue(
    details: ArgumentDetails,
    args: Arguments):
    ResultT[String] = {

    args.args.headOption match {
      case None =>
        ResultTS.leftNE(ArgumentValueMissing(details))

      case Some(v) =>
        val remArgs = args.args.drop(1)
          ResultTS.set(Arguments(remArgs)).map(_ => v)
    }
  }
}