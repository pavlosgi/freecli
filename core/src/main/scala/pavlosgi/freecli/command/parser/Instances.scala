package pavlosgi.freecli.command.parser

import cats.Alternative
import cats.data.NonEmptyList
import cats.syntax.all._

import pavlosgi.freecli.parser.{CliParser, ErrorTermination}

trait Instances {
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
            Left(ErrorTermination(NonEmptyList.of(MultipleCommandsMatched)))
        }
      })
    }

    override def pure[A](x: A): ParseResult[A] = CliParser.success(x)

    override def empty[A]: ParseResult[A] =
      CliParser.error(NoCommandWasMatched)

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
}

