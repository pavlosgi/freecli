package pavlosgi.freecli.command.parser

import cats.data._
import cats.~>

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.command.{help => H}
import pavlosgi.freecli.config.{parser => C}
import pavlosgi.freecli.core.free.FreeAlternative
import pavlosgi.freecli.parser.CliParser

object CommandParserInterpreter extends (Algebra ~> ParseResult) {
  def apply[A](fa: Algebra[A]): ParseResult[A] = {
    fa match {
      case PartialCmd(field, run, f) =>
        for {
          _ <- extractCommandField(field)
        } yield f(PartialCommand(p => Command(field, run(p))))

      case p@PartialCmdWithConfig(field, config, run, f) =>
        for {
          _    <- extractCommandField(field)
          conf <- C.ops.parseConfigNonStrict(config)
            .mapErrors[CommandParsingError](ers =>
              NonEmptyList.of(FailedToParseConfig(field, ers)))

            .mapAction[Action] { c =>
              ConfigAction[A](FreeAlternative.lift(p), H.ops.commandHelp, c)
            }

        } yield f(PartialCommand(p => Command(field, run(conf)(p))))

      case PartialParentCmd(field, subs, f) =>
        for {
          _ <- extractCommandField(field)
          partial <- subs.foldMap(CommandParserInterpreter)
        } yield f(partial)

      case p@PartialParentCmdWithConfig(field, config, subs, f) =>
        for {
          _       <- extractCommandField(field)
          conf <- C.ops.parseConfigNonStrict(config)
            .mapErrors[CommandParsingError](ers =>
              NonEmptyList.of(FailedToParseConfig(field, ers)))

            .mapAction[Action] { c =>
              ConfigAction[A](FreeAlternative.lift(p), H.ops.commandHelp, c)
            }

          partial <- subs.foldMap(CommandParserInterpreter)

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
          CliParser.error[Action, CommandParsingError, Unit](CommandNotFound(field))
      }
    } yield ()
  }
}