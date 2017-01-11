package pavlosgi.freecli.command.parser

import cats.data._
import cats.~>

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.config.{parser => C}
import pavlosgi.freecli.parser.CliParser

object CommandParserInterpreter extends (Algebra ~> ParseResult) {
  def apply[A](fa: Algebra[A]): ParseResult[A] = {
    fa match {
      case PartialCmd(field, run, f) =>
        for {
          _ <- findAndSetCommandArgs(field)
        } yield f(PartialCommand(p => Command(field, run(p))))

      case PartialCmdWithConfig(field, config, run, f) =>
        for {
          _    <- findAndSetCommandArgs(field)
          conf <- config.foldMap(C.ConfigParserInterpreter)
            .leftMap[CommandParsingError](ers => NonEmptyList.of(
            FailedToParseConfig(field, ers)))

          _ <- CliParser.markUnusableBeforeLastUsed[CommandParsingError]

        } yield f(PartialCommand(p => Command(field, run(conf)(p))))

      case PartialParentCmd(field, subs, f) =>
        for {
          _ <- findAndSetCommandArgs(field)
          partial <- subs.foldMap(CommandParserInterpreter)
        } yield f(partial)

      case PartialParentCmdWithConfig(field, config, subs, f) =>
        for {
          _       <- findAndSetCommandArgs(field)
          conf <- config.foldMap(C.ConfigParserInterpreter)
            .leftMap[CommandParsingError](ers => NonEmptyList.of(
            FailedToParseConfig(field, ers)))

          _ <- CliParser.markUnusableBeforeLastUsed[CommandParsingError]
          partial <- subs.foldMap(CommandParserInterpreter)

        } yield f(partial(conf))
    }
  }

  def findAndSetCommandArgs(field: CommandField): ParseResult[Unit] = {
    for {
      res <- CliParser.extractNextIfMatches[CommandParsingError](field.matches)
      _   <- res match {
        case Some(_) =>
          CliParser.success[CommandParsingError, Unit](())

        case None =>
          CliParser.failed[CommandParsingError, Unit](CommandNotFound(field))
      }
    } yield ()
  }
}