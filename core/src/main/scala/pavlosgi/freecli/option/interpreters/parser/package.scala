package pavlosgi.freecli.option.interpreters

import cats.instances.all._
import cats.syntax.all._
import cats.~>

import pavlosgi.freecli.core._
import pavlosgi.freecli.option.api._

package object parser {
  type ParseResult[A] = ResultT[OptionParsingError, CommandLineArguments, A]

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
            args   <- ResultT.get[OptionParsingError, CommandLineArguments]
            value  <- extractOptionFieldAndValue(field, args)
            res    <- parseOpt(field, value, g)
          } yield f(res)

        case Flag(field, f) =>
          for {
            args   <- ResultT.get[OptionParsingError, CommandLineArguments]
            value  <- extractOptionFieldIfExists(field, args)
          } yield f(value)

        case Sub(_, dsl) => dsl.foldMap(optionParserInterpreter)
      }
    }
  }

  def parseOpt[T](field: Field, value: Option[String], g: StringDecoder[T]): ParseResult[Option[T]] = {
    ResultT.fromValidated[StringDecoderError, CommandLineArguments, Option[T]](
      value.traverseU(g.apply)).leftMapInner[OptionParsingError](
        e => FailedToDecodeOption(field, e))
  }

  def extractOptionFieldIfExists(
    field: Field,
    args: CommandLineArguments):
    ParseResult[Boolean] = {

    args.extract(field.matches) match {
      case ExtractSingle(cliArgs, Some(_)) => ResultT.set(cliArgs).map(_ => true)
      case ExtractSingle(cliArgs, None) =>
        val expandedArgs =
          splitAbbreviationsOnFirstMatch(
            field,
            cliArgs.args,
            (arg, abbr) => arg.matches(s"-[a-zA-Z]*$abbr[a-zA-Z]*"))

        if (expandedArgs.diff(cliArgs.args).nonEmpty) {
          extractOptionFieldIfExists(field, CommandLineArguments(expandedArgs))
        } else {
          ResultT.set(cliArgs).map(_ => false)
        }
    }
  }

  def extractOptionFieldAndValue(
    field: Field,
    args: CommandLineArguments):
    ParseResult[Option[String]] = {

    args.extractPair(field.matches) match {
      case ExtractPair(cliArgs, Some(_), Some(v)) =>
        ResultT.set(cliArgs).map(_ => Some(v))

      case ExtractPair(_, Some(_), None) =>
        ResultT.leftNE(OptionFieldValueMissing(field))

      case ExtractPair(cliArgs, None, _) =>
        val expandedArgs =
          splitAbbreviationsOnFirstMatch(
            field,
            cliArgs.args,
            (arg, abbr) => arg.matches(s"-[a-zA-Z][a-zA-Z]*$abbr"))

        if (expandedArgs.diff(cliArgs.args).nonEmpty) {
          extractOptionFieldAndValue(field, CommandLineArguments(expandedArgs))
        } else {
          ResultT.set(cliArgs).map(_ => None)
        }
    }
  }

  def splitAbbreviationsOnFirstMatch(
    field: Field,
    args: Seq[ArgumentWithMarking],
    f: (String, Char) => Boolean):
    Seq[ArgumentWithMarking] = {

    def splitOnMatch(args: Seq[ArgumentWithMarking], abbr: Char): Seq[ArgumentWithMarking] = {
      case class Result(res: Seq[ArgumentWithMarking], found: Boolean)
      val result = args.foldLeft(Result(Seq.empty, found = false)) {
        case (Result(curr, false), arg) if f(arg.value, abbr) =>
          val expanded =
            arg.value.tail.map(c => ArgumentWithMarking(s"-$c", isMarked = false))

          Result(curr ++ expanded, found = true)

        case (r, arg) =>
          r.copy(res = r.res :+ arg)

      }

      result.res
    }

    field match {
      case FieldNameOnly(_, _) => args
      case FieldAbbreviationOnly(abbr, _) =>
        splitOnMatch(args, abbr.abbr)

      case FieldNameAndAbbreviation(_, abbr, _) =>
        splitOnMatch(args, abbr.abbr)
    }
  }
}