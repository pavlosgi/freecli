package pavlosgi.freecli.core.interpreters.parser

import cats.data._
import cats.syntax.all._
import cats.~>

import pavlosgi.freecli.core.api.{StringDecoder, StringDecoderError}
import pavlosgi.freecli.core.api.arguments._
import pavlosgi.freecli.core.dsl.arguments.ArgumentsDsl
import pavlosgi.freecli.core.interpreters.ResultTS

package object arguments {
  type ResultT[A] = ResultTS[ArgumentParsingError, Arguments, A]

  def parseConfig[G, A](
    args: Seq[String])
   (dsl: ArgumentsDsl[A]):
    ValidatedNel[ArgumentParsingError, A] = {

    ResultTS.run(Arguments(args))(dsl.foldMap(argumentsAlgebraParser)) match {
        case (Arguments(Nil), res) => res.toValidated
        case (Arguments(argsLeft), res) =>
          val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(AdditionalArgumentsFound(argsLeft), ers))
      }
  }

  implicit object argumentsAlgebraParser extends (Algebra ~> ResultT) {
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