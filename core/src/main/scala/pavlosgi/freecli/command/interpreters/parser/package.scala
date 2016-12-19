package pavlosgi.freecli.command.interpreters

import cats.data._
import cats.syntax.all._
import cats.{Alternative, ~>}

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.config.interpreters.{parser => C}
import pavlosgi.freecli.parser.CliParser

package object parser {
  type ParseResult[A] = CliParser[CommandParsingError, A]

  implicit def commandParserInterpreter: Algebra ~> ParseResult = {
    new (Algebra ~> ParseResult) {
      def apply[A](fa: Algebra[A]): ParseResult[A] =
        fa match {
          case PartialCmd(field, run, f) =>
            for {
              _ <- findAndSetCommandArgs(field)
            } yield f(PartialCommand(p => Command(field, run(p))))

          case PartialCmdWithConfig(field, config, run, f) =>
            for {
              _    <- findAndSetCommandArgs(field)
              conf <- config.foldMap(C.configParserInterpreter)
                .leftMap[CommandParsingError](ers => NonEmptyList.of(FailedToParseConfig(ers)))

              args <- CliParser.getArgs[CommandParsingError]
              _ <- CliParser.markUnusableBeforeLastUsed[CommandParsingError]

            } yield f(PartialCommand(p => Command(field, run(conf)(p))))

          case PartialParentCmd(field, subs, f) =>
            for {
              _ <- findAndSetCommandArgs(field)
              partial <- subs.foldMap(commandParserInterpreter)(alternativeResultInstance)
            } yield f(partial)

          case PartialParentCmdWithConfig(field, config, subs, f) =>
            for {
              _       <- findAndSetCommandArgs(field)
              conf <- config.foldMap(C.configParserInterpreter)
                .leftMap[CommandParsingError](ers => NonEmptyList.of(FailedToParseConfig(ers)))

              args <- CliParser.getArgs[CommandParsingError]
              _ <- CliParser.markUnusableBeforeLastUsed[CommandParsingError]
              partial <- subs.foldMap(commandParserInterpreter)

            } yield f(partial(conf))
        }
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

  implicit object alternativeResultInstance extends Alternative[ParseResult] {
    override def combineK[A](
      x: ParseResult[A],
      y: ParseResult[A]):
      ParseResult[A] = {

      CliParser.fromState(for {
        xS <- x.value.value
        yS <- y.value.value
      } yield {
        (xS, yS) match {
          case (Right(c1), Left(_)) => Right(c1)
          case (Left(_), Right(c2)) => Right(c2)
          case (Left(c1), Left(c2)) => Left(c1.combine(c2))
          case (Right(_), Right(_)) =>
            Left(NonEmptyList.of(MultipleCommandsMatched))
        }
      })
    }

    override def pure[A](x: A): ParseResult[A] = CliParser.success(x)

    override def empty[A]: ParseResult[A] =
      CliParser.failed(NoCommandWasMatched)

    override def ap[A, B](
      ff: ParseResult[A => B])
     (fa: ParseResult[A]):
      ParseResult[B] = {

      CliParser.fromState(for {
        ff1 <- ff.value.value
        fa1 <- fa.value.value
      } yield {
        fa1.toValidated.ap(ff1.toValidated).toEither
      })
    }
  }
}