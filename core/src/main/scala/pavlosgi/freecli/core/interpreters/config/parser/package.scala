package pavlosgi.freecli.core.interpreters.config

import cats.data._
import cats.std.all._
import cats.syntax.all._

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.interpreters.{Arguments, ResultT}

package object parser {
  def parseConfig[G, A](
    args: Seq[String])
   (dsl: G)
   (implicit ev: G => ResultT[ParsingError, A]):
    ValidatedNel[ParsingError, A] = {

    ResultT.run(Arguments(args))(
      ev(dsl)) match {
        case (Arguments(Nil), res) => res.toValidated
        case (Arguments(argsLeft), res) =>
          val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(UnknownArgumentsParsingError(argsLeft), ers))
      }
  }

  implicit object configAlgebraParser extends Algebra[ResultT[ParsingError, ?]] {

    override def arg[T, A](
      field: Field,
      f: T => A,
      g: StringDecoder[T],
      default: Option[T]):
      ResultT[ParsingError, A] = {

      def mapping(o: Option[T]): ResultT[ParsingError, A] = {
        o.orElse(default) match {
          case Some(s) =>
            ResultT.right[ParsingError, A](f(s))

          case None    =>
            ResultT.leftNE[ParsingError, A](ConfigFieldValueMissingParsingError(field))
        }
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
      ResultT[ParsingError, A] = {

      for {
        args  <- ResultT.get[ParsingError]
        value <- extractConfigFieldStringValue(field, args)
        res   <- ResultT.fromValidated(value.traverseU(v => g.apply(field, v)))
                  .leftMap(_.map[ParsingError](ParsingError.fromStringDecoderError))

      } yield f(res)
    }

    override def flag[A](
      field: Field,
      f: Boolean => A,
      default: Option[Boolean]):
      ResultT[ParsingError, A] = {

      for {
        args  <- ResultT.get[ParsingError]
        value <- extractConfigFieldAndValue(field, args)

        res <- value match {
                 case None =>
                   ResultT.pure[ParsingError, Boolean](default.getOrElse(false))

                 case Some(FieldOnlyOccurrence) =>
                   ResultT.pure[ParsingError, Boolean](true)

                 case Some(FieldAndValueOccurrence(v)) =>
                   ResultT.fromValidated(
                     StringDecoder.booleanStringDecoder(field, v))
                     .leftMap(_.map[ParsingError](ParsingError.fromStringDecoderError))
               }

      } yield f(res)
    }

    override def sub[G, A](
      description: Description,
      dsl: G)
     (implicit ev: G => ResultT[ParsingError, A]):
      ResultT[ParsingError, A] = ev(dsl)

    override def pure[A](x: A): ResultT[ParsingError, A] = ResultT.pure(x)

    override def ap[A, B](
      ff: ResultT[ParsingError, A => B])
     (fa: ResultT[ParsingError, A]):
      ResultT[ParsingError, B] = {

      XorT.apply[State[Arguments, ?], NonEmptyList[ParsingError], B](for {
        ff1 <- ff.value
        fa1 <- fa.value
      } yield fa1.toValidated.ap(ff1.toValidated).toXor)

    }

  }

  def extractConfigFieldAndValue(
    field: Field,
    args: Arguments):
    ResultT[ParsingError, Option[FieldValueOccurrence]] = {

    args.args.indexWhere(field.matches) match {
      case idx if idx === -1 =>
        ResultT.right(None)

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

        ResultT.set[ParsingError](Arguments(remArgs)).map(_ => Some(value))
    }
  }

  def extractConfigFieldStringValue(
    field: Field,
    args: Arguments):
    ResultT[ParsingError, Option[String]] = {

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