package pavlosgi.freecli.core.interpreters.command

import cats.data._
import cats.instances.all._
import cats.syntax.all._
import cats.~>

import pavlosgi.freecli.core.interpreters.{Arguments, ResultT}
import pavlosgi.freecli.core.api.command.{Algebra, Command, CommandField, PartialCommand}
import pavlosgi.freecli.core.interpreters.config.parser.ParsingError

package object parser {
  def parseCommand[G](
    args: Seq[String])
   (dsl: G)
   (implicit ev: G => ResultT[CommandParsingError, Command]):
    ValidatedNel[CommandParsingError, Command] = {

    ResultT.run(Arguments(args))(
      for {
      r <- ev(dsl)
      _ <- validateEmptyArguments
    } yield r)._2.toValidated
  }

  implicit val commandAlgebraParser =
    new Algebra[ResultT[CommandParsingError, ?], ResultT[ParsingError, ?]] {

      implicit def configNat: ResultT[ParsingError, ?] ~> ResultT[CommandParsingError, ?] = {
        new (ResultT[ParsingError, ?] ~> ResultT[CommandParsingError, ?]) {
          def apply[A](fa: ResultT[ParsingError, A]): ResultT[CommandParsingError, A] = {
            fa.leftMap(_.map(FailedToParseConfig))
          }
        }
      }

      def partialCmd[P](
        field: CommandField,
        run: P => Unit):
        ResultT[CommandParsingError, PartialCommand[P]] = {

        for {
          _ <- findAndSetCommandArgs(field)

        } yield PartialCommand[P](p => Command(field, run(p)))
      }

      def partialCmdWithConfig[H[_], A, P](
        field: CommandField,
        config: H[A],
        run: A => P => Unit)
       (implicit ev: H[A] => ResultT[ParsingError, A]):
        ResultT[CommandParsingError, PartialCommand[P]] = {

        for {
          _    <- findAndSetCommandArgs(field)
          conf <- parseConfig(config)

        } yield PartialCommand[P](p => Command(field, run(conf)(p)))
      }

      def partialParentCmd[P, G[_]](
        field: CommandField,
        subs: G[PartialCommand[P]])
       (implicit ev: G[PartialCommand[P]] => ResultT[CommandParsingError, PartialCommand[P]]):
        ResultT[CommandParsingError, PartialCommand[P]] = {

        for {
          cmdArgs <- findAndSetCommandArgs(field)
          partial <- ev(subs)
        } yield partial
      }

      def partialParentCmdWithConfig[H[_], A, P, G[_]](
        field: CommandField,
        config: H[A],
        subs: G[A => PartialCommand[P]])
       (implicit ev: H[A] => ResultT[ParsingError, A],
        ev2: G[A => PartialCommand[P]] => ResultT[CommandParsingError, A => PartialCommand[P]]):
        ResultT[CommandParsingError, PartialCommand[P]] = {

        for {
          _       <- findAndSetCommandArgs(field)
          conf    <- parseConfig(config)
          partial <- ev2(subs)
        } yield partial(conf)
      }

      override def combineK[A](
        x: ResultT[CommandParsingError, A],
        y: ResultT[CommandParsingError, A]):
        ResultT[CommandParsingError, A] = {

        EitherT.apply(for {
          xS <- x.value
          yS <- y.value
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

      override def pure[A](x: A): ResultT[CommandParsingError, A] =
        ResultT.pure(x)

      override def empty[A]: ResultT[CommandParsingError, A] =
        ResultT.leftNE(NoCommandWasMatched)

      override def ap[A, B](
        ff: ResultT[CommandParsingError,
        A => B])
       (fa: ResultT[CommandParsingError, A]):
        ResultT[CommandParsingError, B] = {

        EitherT.apply(for {
          ff1 <- ff.value
          fa1 <- fa.value
        } yield {
          fa1.toValidated.ap(ff1.toValidated).toEither
        })
      }
  }

  def findAndSetCommandArgs(field: CommandField):
    ResultT[CommandParsingError, Unit] = {

    for {
      args <- ResultT.get[CommandParsingError]
      _    <- args.args.indexWhere(field.matches) match {
                case idx if idx === -1 =>
                  ResultT.leftNE[CommandParsingError, Unit](CommandNotFound(field))

                case idx =>
                  ResultT.set[CommandParsingError](Arguments(args.args.drop(idx + 1)))
              }

    } yield ()
  }

  def parseConfig[H[_], A](
    conf: H[A])
   (implicit ev: H[A] => ResultT[ParsingError, A],
    ev2: ResultT[ParsingError, ?] ~> ResultT[CommandParsingError, ?]):
    ResultT[CommandParsingError, A] = {

    ev2.apply(
      for {
        args <- ResultT.get[ParsingError]
        res  <- ev(conf)
    } yield res)
  }

  def validateEmptyArguments: ResultT[CommandParsingError, Unit] = {
    for {
      args <- ResultT.get[CommandParsingError]
      res  <- if (args.args.nonEmpty)
                ResultT.leftNE[CommandParsingError, Unit](
                  UnknownArgumentsCommandParsingError(args.args))

              else ResultT.pure[CommandParsingError, Unit](())

    } yield res
  }
}