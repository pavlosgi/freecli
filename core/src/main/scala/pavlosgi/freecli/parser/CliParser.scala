package pavlosgi.freecli.parser

import cats.Monad
import cats.data._
import cats.instances.all._
import cats.syntax.all._

case class CliArguments(args: Seq[CliArgument]) {
  def usable = args.filter(_.isUsable)
  def unusable = args.filterNot(_.isUsable)
}

case class CliArgument(name: String, isUsable: Boolean)

case class ExtractSingle(res: Option[String])
case class ExtractPair(
  first: Option[String],
  second: Option[String])

case class CliParser[E, A](value: EitherT[State[CliArguments, ?], NonEmptyList[E], A]) {
  def map[B](f: A => B): CliParser[E, B] = {
    CliParser(value.map(f))
  }

  def flatMap[B](f: A => CliParser[E, B]): CliParser[E, B] = {
    CliParser(value.flatMap(f(_).value))
  }

  def leftMapInner[B](f: E => B): CliParser[B, A] = {
    CliParser(value.leftMap(_.map(f)))
  }

  def leftMap[B](f: NonEmptyList[E] => NonEmptyList[B]): CliParser[B, A] = {
    CliParser(value.leftMap(f))
  }
}

object CliParser {
  implicit def monadInstance[E, S] = new Monad[CliParser[E, ?]] {
    def flatMap[A, B](fa: CliParser[E, A])(f: A => CliParser[E, B]): CliParser[E, B] = {
      CliParser(fa.value.flatMap(f(_).value))
    }

    def tailRecM[A, B](a: A)(f: A => CliParser[E, Either[A, B]]): CliParser[E, B] = {
      CliParser(
        implicitly[Monad[EitherT[State[CliArguments, ?], NonEmptyList[E], ?]]]
          .tailRecM(a)(a => f(a).value))
    }

    def pure[A](x: A): CliParser[E, A] = CliParser(EitherT.pure(x))

    override def ap[A, B](ff: CliParser[E, A => B])(fa: CliParser[E, A]): CliParser[E, B] = {
      CliParser(EitherT(
        fa.value.value.flatMap { ei =>
          ff.value.withValidated(f => Validated.fromEither(ei).ap(f)).value
        }))
    }

    override def product[A, B](fa: CliParser[E, A], fb: CliParser[E, B]): CliParser[E, (A, B)] = {
      CliParser.fromState(
        for {
          ei1 <- fa.value.value
          ei2 <- fb.value.value
        } yield {

          (ei1, ei2) match {
            case (Left(ers1), Left(ers2)) =>
              Left(ers1.combine(ers2))

            case (Left(ers1), _) =>
              Left(ers1)

            case (_, Left(ers2)) =>
              Left(ers2)

            case (Right(r1), Right(r2)) =>
              Right(r1 -> r2)
          }
        })
    }
  }

  def run[E, A](
    args: Seq[String])
   (cliParser: CliParser[E, A]):
    (CliArguments, Either[NonEmptyList[E], A]) = {

    cliParser.value.value.run(
      CliArguments(args.map(a => CliArgument(a, isUsable = true)))).value
  }

  def fromState[E, A](
    s: State[CliArguments, Either[NonEmptyList[E], A]]):
    CliParser[E, A] = {

    CliParser(EitherT(s))
  }

  def setArgs[E](args: CliArguments): CliParser[E, Unit] = {
    CliParser(EitherT(State.set[CliArguments](args).map(
      v => Either.right[NonEmptyList[E], Unit](v))))
  }

  def getArgs[E]: CliParser[E, CliArguments] = {
    CliParser(EitherT(State.get[CliArguments].map(Either.right)))
  }

  def success[E, A](v: A): CliParser[E, A] = {
    CliParser(EitherT.fromEither(Right(v)))
  }

  def failed[E, A](error: E): CliParser[E, A] = {
    CliParser(EitherT.fromEither(Left(NonEmptyList.of(error))))
  }

  def fromValidated[E, A](v: ValidatedNel[E, A]): CliParser[E, A] = {
    CliParser(EitherT.fromEither(v.toEither))
  }

  def extractNext[E]: CliParser[E, Option[String]] = {
    for {
      args <- getArgs[E]
      res  = args.args.zipWithIndex.collectFirst {
        case (CliArgument(value, true), idx) =>
          Some(idx) -> Some(value)

      }.getOrElse(None -> None)

      _    <- res._1.traverseU(idx => markUnusable[E](idx))
    } yield res._2
  }

  def extractNextIfMatches[E](f: String => Boolean): CliParser[E, Option[String]] = {
    for {
      args <- getArgs[E]
      res  = args.args.zipWithIndex.find(a => a._1.isUsable) match {
        case Some((a, idx)) if f(a.name) =>
          Some(idx) -> Some(a.name)

        case _ =>
          None -> None
      }

      _    <- res._1.traverseU(idx => markUnusable[E](idx))
    } yield res._2
  }

  def extract[E](f: String => Boolean): CliParser[E, Option[String]] = {
    for {
      args <- getArgs[E]
      res = args.args.zipWithIndex.collectFirst {
        case (CliArgument(value, true), idx) if f(value) =>
          Option(idx) -> Some(value)

      }.getOrElse(None -> None)

      _    <- res._1.traverseU(idx => markUnusable[E](idx))
    } yield res._2
  }

  def extractPair[E](f: String => Boolean): CliParser[E, ExtractPair] = {
    case class Extraction(markUnused: List[Int], extractPair: ExtractPair)

    for {
      args <- getArgs[E]
      res  = args.args.zipWithIndex.sliding(2).collectFirst {
        case List((arg1, idx1), (arg2, idx2)) if arg1.isUsable && arg2.isUsable && f(arg1.name) =>
          Extraction(List(idx1, idx2), ExtractPair(Some(arg1.name), Some(arg2.name)))

        case List((arg1, _)) if arg1.isUsable && f(arg1.name) =>
          Extraction(List.empty, ExtractPair(Some(arg1.name), None))

      }.getOrElse(Extraction(List.empty, ExtractPair(None, None)))

      _    <- res.markUnused.traverseU(idx => markUnusable[E](idx))

    } yield res.extractPair
  }

  def markUnusable[E](idx: Int): CliParser[E, Unit] = {
    for {
      args    <- getArgs[E]
      newArgs = args.args.zipWithIndex.map {
        case (CliArgument(v, _), index) if index === idx =>
          CliArgument(v, isUsable = false)

        case (a, _) => a
      }

      _ <- setArgs(CliArguments(newArgs))

    } yield ()
  }

  def markUnusableBeforeLastUsed[E]: CliParser[E, Unit] = {
    for {
      args <- getArgs[E]
      lastUsed = args.args.filter(_.isUsable).zipWithIndex.map(_._2)
        .lastOption.getOrElse(0)

      newArgs = CliArguments(
        args.args.zipWithIndex.map {
          case (CliArgument(v, _), idx) if idx < lastUsed =>
            CliArgument(v, isUsable = false)

          case (a, _) => a
        })

      _ <- setArgs(newArgs)

    } yield ()
  }
}
