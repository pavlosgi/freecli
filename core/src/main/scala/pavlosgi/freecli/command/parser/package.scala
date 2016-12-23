package pavlosgi.freecli.command

import cats.Alternative
import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._

import pavlosgi.freecli.parser.{CliFailure, CliParser}

package object parser {
  type ParseResult[A] = CliParser[CommandParsingError, A]

  implicit object alternativeResultInstance extends Alternative[ParseResult] {
    override def combineK[A](
      x: ParseResult[A],
      y: ParseResult[A]):
      ParseResult[A] = {

      CliParser.fromEitherState(for {
        xS <- x.value.value
        yS <- y.value.value
      } yield {
        (xS, yS) match {
          case (Right(c1), Left(_)) => Right(c1)
          case (Left(_), Right(c2)) => Right(c2)
          case (Left(c1), Left(c2)) => Left(c1.combine(c2))
          case (Right(_), Right(_)) =>
            Left(CliFailure.errors(NonEmptyList.of(MultipleCommandsMatched)))
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

      CliParser.fromEitherState(for {
        ff1 <- ff.value.value
        fa1 <- fa.value.value
      } yield {
        fa1.toValidated.ap(ff1.toValidated).toEither
      })
    }
  }

  def parseCommand[A](
    args: Seq[String])
   (dsl: CommandDsl[A]):
    Validated[CliFailure[CommandParsingError], A] = {

    val (arguments, res) =
      CliParser.run(args)(
        dsl.foldMap(CommandParserInterpreter)(alternativeResultInstance))

    arguments.usable match {
      case Nil => res.toValidated
      case u =>
        val error = CliFailure.errors[CommandParsingError](
          NonEmptyList.of(AdditionalArgumentsFound(u.map(_.name))))

        res match {
          case Left(failure) =>
            Validated.invalid(failure.combine(error))

          case Right(_) =>
            Validated.invalid(error)
        }
    }
  }
}
