package pavlosgi.freecli.command.interpreters

import cats.data._
import cats.instances.all._
import cats.syntax.all._
import cats.{Alternative, ~>}

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.core.{Arguments, ResultT}
import pavlosgi.freecli.config.interpreters.{parser => C}

package object parser {
  type ParseResult[A] = ResultT[CommandParsingError, Arguments, A]

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
              conf <- configNat(config.foldMap(C.configParserInterpreter))
            } yield f(PartialCommand(p => Command(field, run(conf)(p))))

          case PartialParentCmd(field, subs, f) =>
            for {
              cmdArgs <- findAndSetCommandArgs(field)
              partial <- subs.foldMap(commandParserInterpreter)(alternativeResultInstance)
            } yield f(partial)

          case PartialParentCmdWithConfig(field, config, subs, f) =>
            for {
              _       <- findAndSetCommandArgs(field)
              conf    <- configNat(config.foldMap(C.configParserInterpreter))
              partial <- subs.foldMap(commandParserInterpreter)
            } yield f(partial(conf))
        }
      }
  }

  def configNat: C.ParseResult ~> ParseResult = {
    new (C.ParseResult ~> ParseResult) {
      def apply[A](fa: C.ParseResult[A]): ParseResult[A] = {
        val st =
          fa.value.value.transformS[Arguments](identity, (t, a) =>
            Arguments(a.args))

        ResultT.fromState(st.map(_.leftMap(_.map(FailedToParseConfig))))
      }
    }
  }

  def findAndSetCommandArgs(field: CommandField): ParseResult[Unit] = {
    for {
      st <- ResultT.get[CommandParsingError, Arguments]
      _    <-
        st.args.indexWhere(field.matches) match {
          case idx if idx === -1 =>
            ResultT.leftNE[CommandParsingError, Arguments, Unit](
              CommandNotFound(field))

          case idx =>
            ResultT.set[CommandParsingError, Arguments](
              Arguments(st.args.drop(idx + 1)))
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