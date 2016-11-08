package pavlosgi.freecli.core.interpreters.parser

import cats.data._
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.interpreters.ResultTS

package object config {
  type ResultT[A] = ResultTS[ParsingError, Arguments, A]

  def parseConfig[G, A](
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

    override def requiredOpt[T, A](
      field: Field,
      f: T => A,
      g: StringDecoder[T]):
      ResultT[A] = {

      def mapping(o: Option[T]): ResultT[A] = {
        o match {
          case Some(s) =>
            ResultTS.right(f(s))

          case None    =>
            ResultTS.leftNE(ConfigFieldValueMissingParsingError(field))
        }
      }

      for {
        res <- opt(field, mapping, g)
        v <- res
      } yield v
    }

    override def defaultedOpt[T, A](
      field: Field,
      f: T => A,
      g: StringDecoder[T],
      default: T):
      ResultT[A] = {

      def mapping(o: Option[T]): ResultT[A] = {
        ResultTS.right(o.fold(f(default))(f))
      }

      for {
        res <- opt(field, mapping, g)
        v <- res
      } yield v
    }

    override def opt[T, A](
      field: Field,
      f: Option[T] => A,
      g: StringDecoder[T]):
      ResultT[A] = {

      for {
        args  <- ResultTS.get[ParsingError, Arguments]
        value <- extractConfigFieldStringValue(field, args)
        res   <- ResultTS.fromValidated[StringDecoderError, Arguments, Option[T]](
                  value.traverseU(v => g.apply(field, v)))
                  .leftMap(_.map[ParsingError](ParsingError.fromStringDecoderError))

      } yield f(res)
    }

    override def flag[A](
      field: Field,
      f: Boolean => A,
      default: Boolean = false):
      ResultT[A] = {

      for {
        args  <- ResultTS.get[ParsingError, Arguments]
        value <- extractConfigFieldAndValue(field, args)

        res <- value match {
                 case None =>
                   ResultTS.pure[ParsingError, Arguments, Boolean](default)

                 case Some(FieldOnlyOccurrence) =>
                   ResultTS.pure[ParsingError, Arguments, Boolean](true)

                 case Some(FieldAndValueOccurrence(v)) =>
                   ResultTS.fromValidated[StringDecoderError, Arguments, Boolean](
                     StringDecoder.booleanStringDecoder(field, v))
                     .leftMap(_.map[ParsingError](ParsingError.fromStringDecoderError))
               }

      } yield f(res)
    }

    override def sub[G, A](
      description: Description,
      dsl: G)
     (implicit ev: G => ResultT[A]):
      ResultT[A] = ev(dsl)

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

  def extractConfigFieldAndValue(
    field: Field,
    args: Arguments):
    ResultT[Option[FieldValueOccurrence]] = {

    args.args.indexWhere(field.matches) match {
      case idx if idx === -1 =>
        ResultTS.right(None)

      case idx =>
        val value = args.args.lift(idx + 1).fold(FieldOnlyOccurrence.asBase) { v =>
          if (FieldName.isFieldName(v) || FieldAbbreviation.isFieldAbbreviation(v))
            FieldOnlyOccurrence.asBase
          else
            FieldAndValueOccurrence(v)
        }

        val remArgs = value match {
          case FieldOnlyOccurrence =>
            args.args.take(idx) ++ args.args.drop(idx + 1)

          case FieldAndValueOccurrence(_) =>
            args.args.take(idx) ++ args.args.drop(idx + 2)
        }

        ResultTS.set(Arguments(remArgs)).map(_ => Some(value))
    }
  }

  def extractConfigFieldStringValue(
    field: Field,
    args: Arguments):
    ResultT[Option[String]] = {

    for {
      value <- extractConfigFieldAndValue(field, args)
    } yield {
      value match {
        case None => None
        case Some(FieldOnlyOccurrence) => None
        case Some(FieldAndValueOccurrence(value)) => Some(value)
      }
    }
  }
}