package freecli
package option
package parser

import cats.data.NonEmptyList
import cats.implicits._
import cats.~>
import shapeless.HNil

import core.api.{StringDecoder, StringDecoderError}
import option.api._
import freecli.parser.{CliArgument, CliParser, ExtractPair}

object OptionParserInterpreter extends (Algebra ~> ParseResult) {
  def apply[A](fa: Algebra[A]): ParseResult[A] = {
    fa match {
      case RequiredOpt(field, f, g, default) =>
        def mapping[T](o: Option[T]): ParseResult[A] = {
          o.orElse(default) match {
            case Some(s) =>
              CliParser.success(f(s))

            case None =>
              CliParser.error(NonEmptyList.of(OptionFieldMissing(field)))
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

      case Help(field, f) =>
        for {
          r <- extractOnlyOption(field)
          _ <- if (r) CliParser.action[Action, OptionParsingErrors, Unit](HelpAction)
          else CliParser.success[Action, OptionParsingErrors, Unit](())

        } yield f(HNil)

      case Version(field, value, f) =>
        for {
          r <- extractOnlyOption(field)
          _ <- if (r)
            CliParser.action[Action, OptionParsingErrors, Unit](VersionAction(value))
          else CliParser.success[Action, OptionParsingErrors, Unit](())

        } yield f(HNil)

      case Sub(_, dsl) => dsl.foldMap(OptionParserInterpreter)
    }
  }

  def parseOpt[T](field: OptionField, value: Option[String], g: StringDecoder[T]): ParseResult[Option[T]] = {
    CliParser.fromValidated[Action, NonEmptyList[StringDecoderError], Option[T]](
      value.traverse(g.apply)).mapError[OptionParsingErrors](e =>
        e.map(err => FailedToDecodeOption(field, err)))
  }

  def extractOptionFieldIfExists(field: OptionField): ParseResult[Boolean] = {
    for {
      cliArgs <- CliParser.getArgs[Action, OptionParsingErrors]
      extractedRes <- CliParser.extract[Action, OptionParsingErrors](field.matches)
      res <- extractedRes match {
        case Some(_) =>
          CliParser.success[Action, OptionParsingErrors, Boolean](true)

        case None =>
          val expandedArgs =
            splitAbbreviationsOnFirstMatch(
              field,
              cliArgs,
              (arg, abbr) => arg.matches(s"-[a-zA-Z]*$abbr[a-zA-Z]*"))

          if (expandedArgs.diff(cliArgs).nonEmpty) {
            for {
              _   <- CliParser.setArgs[Action, OptionParsingErrors](expandedArgs)
              res <- extractOptionFieldIfExists(field)
            } yield res

          } else CliParser.success[Action, OptionParsingErrors, Boolean](false)
      }
    } yield res
  }

  def extractOnlyOption(field: OptionField): ParseResult[Boolean] = {
    for {
      extractedRes <- CliParser.extractNextIf[Action, OptionParsingErrors](
        field.matches)

      args <- CliParser.getArgs[Action, OptionParsingErrors]
      res <- (extractedRes, args.filter(_.isUsable)) match {
        case (Some(_), Nil) =>
          CliParser.success[Action, OptionParsingErrors, Boolean](true)

        case (Some(_), a) =>
        CliParser.error[Action, OptionParsingErrors, Boolean](
          NonEmptyList.of(OptionWasFollowedByMoreArguments(field, a.map(_.name))))

        case (None, _) =>
          CliParser.success[Action, OptionParsingErrors, Boolean](false)
      }
    } yield res
  }

  def extractOptionFieldAndValue(field: OptionField): ParseResult[Option[String]] = {
    for {
      cliArgs <- CliParser.getArgs[Action, OptionParsingErrors]
      pair <- CliParser.extractPair[Action, OptionParsingErrors](field.matches)
      res <- pair match {
        case ExtractPair(Some(_), Some(v)) =>
          CliParser.success[Action, OptionParsingErrors, Option[String]](Some(v))

        case ExtractPair(Some(_), None) =>
          CliParser.error[Action, OptionParsingErrors, Option[String]](
            NonEmptyList.of(OptionFieldValueMissing(field)))

        case ExtractPair(None, _) =>
          val expandedArgs =
            splitAbbreviationsOnFirstMatch(
              field,
              cliArgs,
              (arg, abbr) => arg.matches(s"-[a-zA-Z][a-zA-Z]*$abbr"))

          if (expandedArgs.diff(cliArgs).nonEmpty) {
            for {
              _   <- CliParser.setArgs[Action, OptionParsingErrors](expandedArgs)
              res <- extractOptionFieldAndValue(field)
            } yield res

          } else {
            CliParser.success[Action, OptionParsingErrors, Option[String]](None)
          }
      }
    } yield res
  }

  def splitAbbreviationsOnFirstMatch(
    field: OptionField,
    cliArgs: Seq[CliArgument],
    f: (String, Char) => Boolean):
    Seq[CliArgument] = {

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
        splitOnMatch(cliArgs, abbr.abbr)

      case OptionFieldNameAndAbbreviation(_, abbr, _) =>
        splitOnMatch(cliArgs, abbr.abbr)
    }
  }
}