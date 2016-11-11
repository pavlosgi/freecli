package pavlosgi.freecli.core.interpreters.parser

import cats.data._
import cats.syntax.all._

import pavlosgi.freecli.core.api.arguments._
import pavlosgi.freecli.core.interpreters.ResultTS

package object arguments {
  type ResultT[A] = ResultTS[ParsingError, Arguments, A]

  def parseArguments[G, A](
    args: Seq[String])
   (dsl: G)
   (implicit ev: G => ResultT[A]):
    ValidatedNel[ParsingError, A] = {

    ResultTS.run(Arguments(args))(
      ev(dsl)) match {
        case (Arguments(Nil), res) => res.toValidated
        case (Arguments(argsLeft), res) =>
          val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(UnknownArgumentsParsingError(argsLeft), ers))
      }
  }

  implicit object configAlgebraParser extends Algebra[ResultT[?]] {

    override def arg[T, A](
      details: ArgumentDetails,
      f: T => A,
      g: StringDecoder[T]):
      ResultT[A] = {

      for {
        args   <- ResultTS.get[ParsingError, Arguments]
        value  <- extractArgumentValue(details, args)
        res    <- ResultTS.fromValidated[StringDecoderError, Arguments, T](
                    g.apply(details, value)).leftMap(
                    _.map[ParsingError](ParsingError.fromStringDecoderError))

      } yield f(res)
    }

    override def pure[A](x: A): ResultT[A] = ResultTS.pure(x)

    override def ap[A, B](
      ff: ResultT[A => B])
     (fa: ResultT[A]):
      ResultT[B] = {

      EitherT.apply[State[Arguments, ?], NonEmptyList[ParsingError], B](for {
        ff1 <- ff.value
        fa1 <- fa.value
      } yield fa1.toValidated.ap(ff1.toValidated).toEither)

    }
  }

  def extractArgumentValue(
    details: ArgumentDetails,
    args: Arguments):
    ResultT[String] = {

    args.args.headOption match {
      case None =>
        ResultTS.leftNE(ArgumentValueMissingParsingError(details))

      case Some(v) =>
        val remArgs = args.args.drop(1)
          ResultTS.set(Arguments(remArgs)).map(_ => v)
    }
  }
}