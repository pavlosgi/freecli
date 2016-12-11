package pavlosgi.freecli.command.interpreters

import cats.data._
import cats.syntax.all._
import cats.{Alternative, ~>}

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.core.{CommandLineArguments, ExtractSingle, ResultT}
import pavlosgi.freecli.config.interpreters.{parser => C}

package object parser {
  type ParseResult[A] = ResultT[CommandParsingError, CommandLineArguments, A]

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

              args <- ResultT.get[CommandParsingError, CommandLineArguments]
              _ <- ResultT.set(args.markAllBeforeLastMarked)

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

              args <- ResultT.get[CommandParsingError, CommandLineArguments]
              _ <- ResultT.set(args.markAllBeforeLastMarked)
              partial <- subs.foldMap(commandParserInterpreter)

            } yield f(partial(conf))
        }
      }
  }

  def findAndSetCommandArgs(field: CommandField): ParseResult[Unit] = {
    for {
      cliArgs <- ResultT.get[CommandParsingError, CommandLineArguments]
      _  <- cliArgs.extractNextIfMatches(field.matches) match {
        case ExtractSingle(updated, Some(_)) =>
          ResultT.set[CommandParsingError, CommandLineArguments](updated)

        case ExtractSingle(_, None) =>
          ResultT.leftNE[CommandParsingError, CommandLineArguments, Unit](
            CommandNotFound(field))
      }
    } yield ()
  }

  implicit object alternativeResultInstance extends Alternative[ParseResult] {
    override def combineK[A](
      x: ParseResult[A],
      y: ParseResult[A]):
      ParseResult[A] = {

      ResultT.fromState(for {
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

    override def pure[A](x: A): ParseResult[A] =
      ResultT.pure(x)

    override def empty[A]: ParseResult[A] =
      ResultT.leftNE(NoCommandWasMatched)

    override def ap[A, B](
      ff: ParseResult[A => B])
     (fa: ParseResult[A]):
      ParseResult[B] = {

      ResultT.fromState(for {
        ff1 <- ff.value.value
        fa1 <- fa.value.value
      } yield {
        fa1.toValidated.ap(ff1.toValidated).toEither
      })
    }
  }
}