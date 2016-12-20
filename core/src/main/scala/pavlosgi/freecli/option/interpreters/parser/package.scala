package pavlosgi.freecli.option.interpreters

import cats.instances.all._
import cats.syntax.all._
import cats.~>

import pavlosgi.freecli.core.{StringDecoder, StringDecoderError}
import pavlosgi.freecli.option.api._
import pavlosgi.freecli.parser.{CliArgument, CliArguments, CliParser, ExtractPair}

package object parser {
  type ParseResult[A] = CliParser[OptionParsingError, A]

  implicit object optionParserInterpreter extends (Algebra ~> ParseResult) {
    def apply[A](fa: Algebra[A]): ParseResult[A] = {
      fa match {
        case RequiredOpt(field, f, g, default) =>
          def mapping[T](o: Option[T]): ParseResult[A] = {
            o.orElse(default) match {
              case Some(s) =>
                CliParser.success(f(s))

              case None =>
                CliParser.failed(OptionFieldMissing(field))
            }
          }

          for {
            res <- apply(Opt(field, mapping, g))
            v <- res
          } yield v

        case Opt(field, f, g) =>
          for {
            value  <- extractOptionFieldAndValue(field)
            res    <- parseOpt(field, value, g)
          } yield f(res)

        case Flag(field, f) =>
          extractOptionFieldIfExists(field).map(f)

        case Sub(_, dsl) => dsl.foldMap(optionParserInterpreter)
      }
    }
  }

  def parseOpt[T](field: OptionField, value: Option[String], g: StringDecoder[T]): ParseResult[Option[T]] = {
    CliParser.fromValidated[StringDecoderError, Option[T]](
      value.traverseU(g.apply)).leftMapInner[OptionParsingError](
        e => FailedToDecodeOption(field, e))
  }

  def extractOptionFieldIfExists(field: OptionField): ParseResult[Boolean] = {
    for {
      cliArgs <- CliParser.getArgs[OptionParsingError]
      extractedRes <- CliParser.extract(field.matches)
      res <- extractedRes match {
        case Some(_) =>
          CliParser.success[OptionParsingError, Boolean](true)

        case None =>
          val expandedArgs =
            splitAbbreviationsOnFirstMatch(
              field,
              cliArgs,
              (arg, abbr) => arg.matches(s"-[a-zA-Z]*$abbr[a-zA-Z]*"))

          if (expandedArgs.args.diff(cliArgs.args).nonEmpty) {
            for {
              _   <- CliParser.setArgs(expandedArgs)
              res <- extractOptionFieldIfExists(field)
            } yield res

          } else CliParser.success[OptionParsingError, Boolean](false)
      }
    } yield res
  }

  def extractOptionFieldAndValue(field: OptionField): ParseResult[Option[String]] = {
    for {
      cliArgs <- CliParser.getArgs[OptionParsingError]
      pair <- CliParser.extractPair[OptionParsingError](field.matches)
      res <- pair match {
        case ExtractPair(Some(_), Some(v)) =>
          CliParser.success[OptionParsingError, Option[String]](Some(v))

        case ExtractPair(Some(_), None) =>
          CliParser.failed[OptionParsingError, Option[String]](
            OptionFieldValueMissing(field))

        case ExtractPair(None, _) =>
          val expandedArgs =
            splitAbbreviationsOnFirstMatch(
              field,
              cliArgs,
              (arg, abbr) => arg.matches(s"-[a-zA-Z][a-zA-Z]*$abbr"))

          if (expandedArgs.args.diff(cliArgs.args).nonEmpty) {
            for {
              _   <- CliParser.setArgs(expandedArgs)
              res <- extractOptionFieldAndValue(field)
            } yield res

          } else {
            CliParser.success[OptionParsingError, Option[String]](None)
          }
      }
    } yield res
  }

  def splitAbbreviationsOnFirstMatch(
    field: OptionField,
    cliArgs: CliArguments,
    f: (String, Char) => Boolean):
    CliArguments = {

    def splitOnMatch(args: Seq[CliArgument], abbr: Char): Seq[CliArgument] = {
      case class Result(res: Seq[CliArgument], found: Boolean)
      val result = args.foldLeft(Result(Seq.empty, found = false)) {
        case (Result(curr, false), arg) if arg.isUsable && f(arg.name, abbr) =>
          val expanded =
            arg.name.tail.map(c => CliArgument(s"-$c", isUsable = true))

          Result(curr ++ expanded, found = true)

        case (r, arg) =>
          r.copy(res = r.res :+ arg)

      }

      result.res
    }

    field match {
      case OptionFieldNameOnly(_, _) => cliArgs
      case OptionFieldAbbreviationOnly(abbr, _) =>
        CliArguments(splitOnMatch(cliArgs.args, abbr.abbr))

      case OptionFieldNameAndAbbreviation(_, abbr, _) =>
        CliArguments(splitOnMatch(cliArgs.args, abbr.abbr))
    }
  }
}