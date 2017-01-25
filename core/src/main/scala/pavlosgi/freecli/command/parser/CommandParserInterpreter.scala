package pavlosgi.freecli.command.parser

import cats.~>

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.command.{help => H}
import pavlosgi.freecli.config.{parser => C}
import pavlosgi.freecli.core.free.FreeAlternative
import pavlosgi.freecli.parser.CliParser

object CommandParserInterpreter extends (Algebra ~> ParseResult) {
  def apply[A](fa: Algebra[A]): ParseResult[A] = {
    fa match {
      case p@PartialCmd(field, run, f) =>
        for {
          _ <- extractCommandField(field)
          _ <- CliParser.setFailMessage[Action, CommandParsingError](
            H.ops.commandHelp(FreeAlternative.lift(p)))

        } yield f(PartialCommand(p => Command(field, run(p))))

      case p@PartialCmdWithConfig(field, config, run, f) =>
        for {
          _    <- extractCommandField(field)
          _ <- CliParser.setFailMessage[Action, CommandParsingError](
            H.ops.commandHelp(FreeAlternative.lift(p)))

          conf <- C.ops.parseConfigNonStrict(config).mapError[CommandParsingError](ers =>
              OtherCommandErrors(failedToParseConfig = Some(FailedToParseConfig(field, ers))))

            .mapAction[Action] { c =>
              ConfigAction[A](FreeAlternative.lift(p), H.ops.commandHelp, c)
            }

        } yield f(PartialCommand(c => Command(field, run(conf)(c))))

      case p@PartialParentCmd(field, subs, f) =>
        for {
          _ <- extractCommandField(field)
          _ <- CliParser.setFailMessage[Action, CommandParsingError](
            H.ops.commandHelp(FreeAlternative.lift(p)))

          partial <- subs.foldMap(CommandParserInterpreter).mapError[CommandParsingError] { ers =>
            ParentCommandError(field, ers)
          }

        } yield f(partial)

      case p@PartialParentCmdWithConfig(field, config, subs, f) =>
        for {
          _    <- extractCommandField(field)
          _ <- CliParser.setFailMessage[Action, CommandParsingError](
            H.ops.commandHelp(FreeAlternative.lift(p)))

          conf <- C.ops.parseConfigNonStrict(config).mapError[CommandParsingError](ers =>
              OtherCommandErrors(failedToParseConfig = Some(FailedToParseConfig(field, ers))))

            .mapAction[Action] { c =>
              ConfigAction[A](FreeAlternative.lift(p), H.ops.commandHelp, c)
            }

          partial <- subs.foldMap(CommandParserInterpreter).mapError[CommandParsingError] { ers =>
            ParentCommandError(field, ers)
          }

        } yield f(partial(conf))
    }
  }

  def extractCommandField(field: CommandField): ParseResult[Unit] = {
    for {
      res <- CliParser.extractNextIf[Action, CommandParsingError](field.matches)
      _   <- res match {
        case Some(_) =>
          CliParser.success[Action, CommandParsingError, Unit](())

        case None =>
          CliParser.error[Action, CommandParsingError, Unit](
            OtherCommandErrors(commandNotFound = List(CommandNotFound(field))))
      }
    } yield ()
  }
}