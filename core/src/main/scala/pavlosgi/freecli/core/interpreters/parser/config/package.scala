package pavlosgi.freecli.core.interpreters.parser

import cats.data._
import cats.instances.all._
import cats.syntax.all._
import cats.~>

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.ConfigDsl
import pavlosgi.freecli.core.interpreters.ResultTS

package object config {
  type ResultT[A] = ResultTS[ParsingError, Arguments, A]

  def parseConfig[G, A](
    args: Seq[String])
   (dsl: ConfigDsl[A]):
    ValidatedNel[ParsingError, A] = {

    ResultTS.run(Arguments(args))(dsl.foldMap(configAlgebraParser)) match {
        case (Arguments(Nil), res) => res.toValidated
        case (Arguments(argsLeft), res) =>
          val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(UnknownArgumentsParsingError(argsLeft), ers))
      }
  }

  implicit object configAlgebraParser extends (Algebra ~> ResultT) {
    def apply[A](fa: Algebra[A]): ResultT[A] = {
      fa match {
        case Arg(details, f, g) =>
          for {
            args   <- ResultTS.get[ParsingError, Arguments]
            value  <- extractArgumentValue(details, args)
            res    <- parseArg(value, g)
          } yield f(res)

        case RequiredOpt(field, f, g, default) =>
          def mapping[T](o: Option[T]): ResultT[A] = {
            o.orElse(default) match {
              case Some(s) =>
                ResultTS.right(f(s))

              case None =>
                ResultTS.leftNE(OptionFieldMissingParsingError(field))
            }
          }

          for {
            res <- apply(Opt(field, mapping, g))
            v <- res
          } yield v

        case Opt(field, f, g) =>
          for {
            args   <- ResultTS.get[ParsingError, Arguments]
            value  <- extractOptionFieldAndValue(field, args)
            res    <- parseOpt(value, g)
          } yield f(res)

        case Flag(field, f) =>
          for {
            args   <- ResultTS.get[ParsingError, Arguments]
            value  <- extractOptionFieldIfExists(field, args)
          } yield f(value)

        case Sub(description, dsl) => dsl.foldMap(configAlgebraParser)
      }
    }
  }

  def parseArg[T](value: String, g: StringDecoder[T]) = {
    ResultTS.fromValidated[StringDecoderError, Arguments, T](
      g.apply(value)).leftMap(
      _.map[ParsingError](ParsingError.fromStringDecoderError))
  }

  def parseOpt[T](value: Option[String], g: StringDecoder[T]) = {
    ResultTS.fromValidated[StringDecoderError, Arguments, Option[T]](
      value.traverseU(v => g.apply(v)))
      .leftMap(_.map[ParsingError](ParsingError.fromStringDecoderError))
  }

  def extractOptionFieldIfExists(
    field: Field,
    args: Arguments):
    ResultT[Boolean] = {

    args.args.indexWhere(field.matches) match {
      case idx if idx === -1 =>
        tryBySplittingArgs(
          field,
          args,
          extractOptionFieldIfExists,
          ResultTS.right(false))

      case idx =>
        val remArgs = args.args.take(idx) ++ args.args.drop(idx + 1)
        ResultTS.set(Arguments(remArgs)).map(_ => true)
    }
  }

  def extractOptionFieldAndValue(
    field: Field,
    args: Arguments):
    ResultT[Option[String]] = {

    args.args.indexWhere(field.matches) match {
      case idx if idx === -1 =>
        tryBySplittingArgs(
          field,
          args,
          extractOptionFieldAndValue,
          ResultTS.right(None))

      case idx =>
        args.args.lift(idx + 1) match {
          case None =>
            ResultTS.leftNE(OptionFieldValueMissingParsingError(field))

          case Some(v) =>
            val remArgs = args.args.take(idx) ++ args.args.drop(idx + 2)
            ResultTS.set(Arguments(remArgs)).map(_ => Some(v))
        }
    }
  }

  def tryBySplittingArgs[T](
    field: Field,
    args: Arguments,
    f: (Field, Arguments) => ResultT[T],
    fallback: ResultT[T]) = {

    val newArgs = args.args.foldLeft(Seq.empty[String]) {
      case (curr, arg) =>
        curr ++ FieldAbbreviation.splitMultiFieldAbbreviation(arg)
    }

    if (newArgs.diff(args.args).isEmpty) {
      fallback
    } else {
      f(field, Arguments(newArgs))
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