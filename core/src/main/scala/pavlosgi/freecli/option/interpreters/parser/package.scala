package pavlosgi.freecli.option.interpreters

import cats.instances.all._
import cats.syntax.all._
import cats.~>

import pavlosgi.freecli.core._
import pavlosgi.freecli.option.api._

package object parser {
  type ParseResult[A] = ResultT[OptionParsingError, Arguments, A]

  implicit object optionParserInterpreter extends (Algebra ~> ParseResult) {
    def apply[A](fa: Algebra[A]): ParseResult[A] = {
      fa match {
        case RequiredOpt(field, f, g, default) =>
          def mapping[T](o: Option[T]): ParseResult[A] = {
            o.orElse(default) match {
              case Some(s) =>
                ResultT.right(f(s))

              case None =>
                ResultT.leftNE(OptionFieldMissing(field))
            }
          }

          for {
            res <- apply(Opt(field, mapping, g))
            v <- res
          } yield v

        case Opt(field, f, g) =>
          for {
            args   <- ResultT.get[OptionParsingError, Arguments]
            value  <- extractOptionFieldAndValue(field, args)
            res    <- parseOpt(field, value, g)
          } yield f(res)

        case Flag(field, f) =>
          for {
            args   <- ResultT.get[OptionParsingError, Arguments]
            value  <- extractOptionFieldIfExists(field, args)
          } yield f(value)

        case Sub(description, dsl) => dsl.foldMap(optionParserInterpreter)
      }
    }
  }

  def parseOpt[T](field: Field, value: Option[String], g: StringDecoder[T]) = {
    ResultT.fromValidated[StringDecoderError, Arguments, Option[T]](
      value.traverseU(g.apply)).leftMapInner[OptionParsingError](
        e => FailedToDecodeOption(field, e))
  }

  def extractOptionFieldIfExists(
    field: Field,
    args: Arguments):
    ParseResult[Boolean] = {

    args.args.indexWhere(a => field.matches(a.arg)) match {
      case idx if idx === -1 =>
        tryBySplittingArgs(
          field,
          args,
          extractOptionFieldIfExists,
          ResultT.right(false))

      case idx =>
        val remArgs = args.args.take(idx) ++ args.args.drop(idx + 1)
        ResultT.set(Arguments(remArgs)).map(_ => true)
    }
  }

  def extractOptionFieldAndValue(
    field: Field,
    args: Arguments):
    ParseResult[Option[String]] = {

    args.args.indexWhere(a => field.matches(a.arg)) match {
      case idx if idx === -1 =>
        tryBySplittingArgs(
          field,
          args,
          extractOptionFieldAndValue,
          ResultT.right(None))

      case idx =>
        args.args.lift(idx + 1) match {
          case None =>
            ResultT.leftNE(OptionFieldValueMissing(field))

          case Some(v) =>
            val newArgs = args.marked(idx).marked(idx + 1)
            ResultT.set(newArgs).map(_ => Some(v.arg))
        }
    }
  }

  def tryBySplittingArgs[T](
    field: Field,
    args: Arguments,
    f: (Field, Arguments) => ParseResult[T],
    fallback: ParseResult[T]) = {

    val newArgs =
      args.copy(args = args.args.foldLeft(Seq.empty[Argument]) {
        case (c, Argument(arg, Unmarked)) =>
          val split =
            FieldAbbreviation.splitMultiFieldAbbreviation(arg)
              .map(a => Argument(a, Unmarked))

          c ++ split

        case (c, other) => c :+ other
      })

    if (newArgs.args.size === args.args.size) {
      fallback
    } else {
      f(field, newArgs)
    }
  }
}