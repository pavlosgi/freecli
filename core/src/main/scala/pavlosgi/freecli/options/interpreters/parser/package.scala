package pavlosgi.freecli.options.interpreters

import cats.data._
import cats.instances.all._
import cats.syntax.all._
import cats.~>

import pavlosgi.freecli.core.{Arguments, ResultTS, StringDecoder, StringDecoderError}
import pavlosgi.freecli.core.{ResultTS, StringDecoder, StringDecoderError}
import pavlosgi.freecli.options.api._
import pavlosgi.freecli.options.dsl.OptionsDsl

package object parser {
  type ResultT[A] = ResultTS[OptionParsingError, Arguments, A]

  def parseOptions[G, A](
    args: Seq[String])
   (dsl: OptionsDsl[A]):
    ValidatedNel[OptionParsingError, A] = {

    ResultTS.run(Arguments(args))(dsl.foldMap(optionAlgebraParser)) match {
        case (Arguments(Nil), res) => res.toValidated
        case (Arguments(argsLeft), res) =>
          val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(AdditionalOptionsFound(argsLeft), ers))
      }
  }

  implicit object optionAlgebraParser extends (Algebra ~> ResultT) {
    def apply[A](fa: Algebra[A]): ResultT[A] = {
      fa match {
        case RequiredOpt(field, f, g, default) =>
          def mapping[T](o: Option[T]): ResultT[A] = {
            o.orElse(default) match {
              case Some(s) =>
                ResultTS.right(f(s))

              case None =>
                ResultTS.leftNE(OptionFieldMissing(field))
            }
          }

          for {
            res <- apply(Opt(field, mapping, g))
            v <- res
          } yield v

        case Opt(field, f, g) =>
          for {
            args   <- ResultTS.get[OptionParsingError, Arguments]
            value  <- extractOptionFieldAndValue(field, args)
            res    <- parseOpt(field, value, g)
          } yield f(res)

        case Flag(field, f) =>
          for {
            args   <- ResultTS.get[OptionParsingError, Arguments]
            value  <- extractOptionFieldIfExists(field, args)
          } yield f(value)

        case Sub(description, dsl) => dsl.foldMap(optionAlgebraParser)
      }
    }
  }

  def parseOpt[T](field: Field, value: Option[String], g: StringDecoder[T]) = {
    ResultTS.fromValidated[StringDecoderError, Arguments, Option[T]](
      value.traverseU(v => g.apply(v)))
      .leftMap(_.map[OptionParsingError] { err =>
        FailedToDecodeOption(field, err)
      })
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
            ResultTS.leftNE(OptionFieldValueMissing(field))

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
}