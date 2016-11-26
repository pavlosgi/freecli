package pavlosgi.freecli.core.interpreters.parser

import cats.data._
import cats.instances.all._
import cats.syntax.all._
import cats.{Alternative, ~>}

import pavlosgi.freecli.core.api.command._
import pavlosgi.freecli.core.dsl.command.CommandDsl
import pavlosgi.freecli.core.interpreters.parser.{config => C}
import pavlosgi.freecli.core.interpreters.ResultTS

package object command {
  type ResultT[A] = ResultTS[CommandParsingError, Arguments, A]

  def parseCommand[A](
    args: Seq[String])
   (dsl: CommandDsl[A]):
    ValidatedNel[CommandParsingError, A] = {

    val resultT: ResultT[A] =
      dsl.foldMap(commandAlgebraParser)(alternativeResultInstance)

    ResultTS.run[CommandParsingError, Arguments, A](Arguments(args))(resultT) match {
      case (Arguments(Nil), res) => res.toValidated
      case (Arguments(argsLeft), res) =>
        val ers = res.fold(_.toList, _ => List.empty)
        Validated.invalid(
          NonEmptyList(UnknownArgumentsCommandParsingError(argsLeft), ers))
    }
  }

  implicit def commandAlgebraParser: Algebra ~> ResultT = {
    new (Algebra ~> ResultT) {
      def apply[A](fa: Algebra[A]): ResultT[A] =
        fa match {
          case PartialCmd(field, run, f) =>
            for {
              _ <- findAndSetCommandArgs(field)
            } yield f(PartialCommand(p => Command(field, run(p))))

          case PartialCmdWithConfig(field, config, run, f) =>
            for {
              _    <- findAndSetCommandArgs(field)
              conf <- configNat(config.foldMap(C.configAlgebraParser))
            } yield f(PartialCommand(p => Command(field, run(conf)(p))))

          case PartialParentCmd(field, subs, f) =>
            for {
              cmdArgs <- findAndSetCommandArgs(field)
              partial <- subs.foldMap(commandAlgebraParser)
            } yield f(partial)

          case PartialParentCmdWithConfig(field, config, subs, f) =>
            for {
              _       <- findAndSetCommandArgs(field)
              conf    <- configNat(config.foldMap(C.configAlgebraParser))
              partial <- subs.foldMap(commandAlgebraParser)
            } yield f(partial(conf))
        }
      }
  }

  def configNat: C.ResultT ~> ResultT = {
    new (C.ResultT ~> ResultT) {
      def apply[A](fa: C.ResultT[A]): ResultT[A] = {
        val st =
          fa.value.transformS[Arguments](identity, (t, a) =>
            Arguments(a.args))

        EitherT.apply(st.map(_.leftMap(_.map(FailedToParseConfig))))
      }
    }
  }

  def findAndSetCommandArgs(field: CommandField): ResultT[Unit] = {
    for {
      st <- ResultTS.get[CommandParsingError, Arguments]
      _    <-
        st.args.indexWhere(field.matches) match {
          case idx if idx === -1 =>
            ResultTS.leftNE[CommandParsingError, Arguments, Unit](
              CommandNotFound(field))

          case idx =>
            ResultTS.set[CommandParsingError, Arguments](
              Arguments(st.args.drop(idx + 1)))
        }

    } yield ()
  }

  implicit object alternativeResultInstance extends Alternative[ResultT] {
    override def combineK[A](
      x: ResultT[A],
      y: ResultT[A]):
      ResultT[A] = {

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

    override def pure[A](x: A): ResultT[A] =
      EitherT.pure(x)

    override def empty[A]: ResultT[A] =
      ResultTS.leftNE(NoCommandWasMatched)

    override def ap[A, B](
      ff: ResultT[A => B])
     (fa: ResultT[A]):
      ResultT[B] = {

      EitherT.apply(for {
        ff1 <- ff.value
        fa1 <- fa.value
      } yield {
        fa1.toValidated.ap(ff1.toValidated).toEither
      })
    }
  }
}