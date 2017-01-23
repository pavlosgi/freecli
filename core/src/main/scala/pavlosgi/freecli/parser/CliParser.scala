package pavlosgi.freecli.parser

import cats.Monad
import cats.data._
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core.formatting._

case class CliParser[A, E, T](value: EitherT[State[CliArguments, ?], EarlyTermination[A, E], T]) {
  def map[B](f: T => B): CliParser[A, E, B] = {
    CliParser(value.map(f))
  }

  def flatMap[B](f: T => CliParser[A, E, B]): CliParser[A, E, B] = {
    CliParser(value.flatMap(f(_).value))
  }

  def mapError[B](f: E => B): CliParser[A, B, T] = {
    CliParser(value.leftMap {
      case ErrorTermination(errors) => ErrorTermination(errors.map(f))
      case ActionTermination(a) => ActionTermination(a)
    })
  }

  def mapErrors[B](f: NonEmptyList[E] => NonEmptyList[B]): CliParser[A, B, T] = {
    CliParser(value.leftMap {
      case ErrorTermination(ers) => ErrorTermination(f(ers))
      case ActionTermination(a) => ActionTermination(a)
    })
  }

  def mapAction[Ac2](f: A => Ac2): CliParser[Ac2, E, T] = {
    CliParser(value.leftMap {
      case ActionTermination(a) => ActionTermination(f(a))
      case ErrorTermination(ers) => ErrorTermination(ers)
    })
  }

  def failIfNotAllArgumentsUsed(e: CliArguments => E): CliParser[A, E, T] = {
    val st = for {
      res <- value.value
      args <- value.value.get
    } yield {
      res match {
        case Left(ErrorTermination(ers)) if args.usable.nonEmpty =>
          Left[ErrorTermination[A, E], T](ErrorTermination(ers ++ List(e(args))))

        case _ if args.usable.nonEmpty =>
          Left[ErrorTermination[A, E], T](ErrorTermination(NonEmptyList.of(e(args))))

        case r => r
      }
    }

    CliParser(EitherT[State[CliArguments, ?], EarlyTermination[A, E], T](st))
  }

  def run(args: Seq[String]): Result[A, E, T] = {
    val (_, res) = CliParser.run[A, E, T](args)(this)
    res match {
      case Right(v) => Success[A, E, T](v)
      case Left(ActionTermination(a)) => Action[A, E, T](a)
      case Left(ErrorTermination(e)) => Failure[A, E, T](e)
    }
  }
}

object CliParser {
  implicit def monadInstance[A, E, S] = new Monad[CliParser[A, E, ?]] {
    def flatMap[T, B](fa: CliParser[A, E, T])(f: T => CliParser[A, E, B]): CliParser[A, E, B] = {
      CliParser(fa.value.flatMap(f(_).value))
    }

    def tailRecM[T, B](a: T)(f: T => CliParser[A, E, Either[T, B]]): CliParser[A, E, B] = {
      CliParser(
        implicitly[Monad[EitherT[State[CliArguments, ?], EarlyTermination[A, E], ?]]]
          .tailRecM(a)(a => f(a).value))
    }

    def pure[T](x: T): CliParser[A, E, T] = CliParser(EitherT.pure(x))

    override def ap[T, B](ff: CliParser[A, E, T => B])(fa: CliParser[A, E, T]): CliParser[A, E, B] = {
      CliParser(EitherT(
        fa.value.value.flatMap { ei =>
          ff.value.withValidated(f => Validated.fromEither(ei).ap(f)).value
        }))
    }

    override def product[T, B](fa: CliParser[A, E, T], fb: CliParser[A, E, B]): CliParser[A, E, (T, B)] = {
      CliParser.fromEitherState(
        for {
          ei1 <- fa.value.value
          ei2 <- fb.value.value
        } yield {

          (ei1, ei2) match {
            case (Left(e1), Left(e2)) =>
              Left(e1.combine(e2))

            case (Left(e1), Right(_)) =>
              Left(e1)

            case (Right(_), Left(e2)) =>
              Left(e2)

            case (Right(r1), Right(r2)) =>
              Right(r1 -> r2)
          }
        })
    }
  }

  def run[A, E, T](
    args: Seq[String])
   (cliParser: CliParser[A, E, T]):
    (CliArguments, Either[EarlyTermination[A, E], T]) = {

    cliParser.value.value.run(CliArguments(
      args.map(a => CliArgument(a, isUsable = true)))).value
  }

  def runOrFail[A, E, T](
    args: Seq[String],
    help: String,
    action: A => Unit)
   (cliParser: CliParser[A, E, T])
   (implicit ev: Error[E]):
    T = {

    val (arguments, res) = run(args)(cliParser)
    res match {
      case Right(v)  => v
      case Left(ActionTermination(a)) =>
        action(a)
        sys.exit(1)

      case Left(ErrorTermination(ers)) =>
        val errorsDisplay =
          s"""
           |${"Errors:".underline.bold.red}
           |
           |${indent(2, ers.toList.map(ev.message).mkString("\n"))}""".stripMargin

        val parsingDetails = arguments.args.map {
          case CliArgument(name, false) =>
            name.yellow

          case a => a.name
        }.mkString(" ")

        val parsingDetailDisplay =
          s"""${"Parsing details:".underline.bold.red + s" (${"used".yellow}, unused)"}
             |
             |$parsingDetails
           """.stripMargin

        println(
          s"""$errorsDisplay
             |
             |$parsingDetailDisplay
             |$help
             |""".stripMargin)

        sys.exit(1)
    }
  }

  def fromState[A, E, T](
    s: State[CliArguments, T]):
    CliParser[A, E, T] = {

    CliParser(EitherT(s.map(Either.right)))
  }

  def fromEitherState[A, E, T](
    s: State[CliArguments, Either[EarlyTermination[A, E], T]]):
    CliParser[A, E, T] = {

    CliParser(EitherT(s))
  }

  def get[A, E]: CliParser[A, E, CliArguments] = {
    CliParser[A, E, CliArguments](EitherT(State.get[CliArguments].map(Either.right)))
  }

  def set[A, E](state: CliArguments): CliParser[A, E, Unit] = {
    fromState(State.set(state))
  }

  def modify[A, E](f: CliArguments => CliArguments): CliParser[A, E, Unit] = {
    fromState(State.modify[CliArguments](f))
  }

  def setArgs[A, E](args: CliArguments): CliParser[A, E, Unit] = {
    set(args)
  }

  def getArgs[A, E]: CliParser[A, E, CliArguments] = {
    get[A, E]
  }

  def success[A, E, T](v: T): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(Right(v)))
  }

  def error[A, E, T](error: E): CliParser[A, E, T] = {
    errors(NonEmptyList.of(error))
  }

  def errors[A, E, T](errors: NonEmptyList[E]): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(Left(ErrorTermination(errors))))
  }

  def action[A, E, T](action: A): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(Left(ActionTermination(action))))
  }

  def fromEither[A, E, T](v: Either[NonEmptyList[E], T]): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(v.leftMap(e => ErrorTermination[A, E](e))))
  }

  def fromValidated[A, E, T](v: ValidatedNel[E, T]): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(v.toEither.leftMap(e => ErrorTermination[A, E](e))))
  }

  def extractNext[A, E]: CliParser[A, E, Option[String]] = {
    for {
      args <- getArgs[A, E]
      res  = args.args.zipWithIndex.collectFirst {
        case (CliArgument(value, true), idx) =>
          Some(idx) -> Some(value)

      }.getOrElse(None -> None)

      _    <- res._1.traverseU(idx => markUnusable[A, E](idx))
    } yield res._2
  }

  def extractNextIf[A, E](f: String => Boolean): CliParser[A, E, Option[String]] = {
    for {
      args <- getArgs[A, E]
      res  = args.args.zipWithIndex.find(a => a._1.isUsable) match {
        case Some((a, idx)) if f(a.name) =>
          Some(idx) -> Some(a.name)

        case _ =>
          None -> None
      }

      _    <- res._1.traverseU(idx => markUnusable[A, E](idx))
    } yield res._2
  }

  def extract[A, E](f: String => Boolean): CliParser[A, E, Option[String]] = {
    for {
      args <- getArgs[A, E]
      res = args.args.zipWithIndex.collectFirst {
        case (CliArgument(value, true), idx) if f(value) =>
          Option(idx) -> Some(value)

      }.getOrElse(None -> None)

      _    <- res._1.traverseU(idx => markUnusable[A, E](idx))
    } yield res._2
  }

  def extractPair[A, E](f: String => Boolean): CliParser[A, E, ExtractPair] = {
    case class Extraction(markUnused: List[Int], extractPair: ExtractPair)

    for {
      args <- getArgs[A, E]
      res  = args.args.zipWithIndex.sliding(2).map(_.toList).collectFirst {
        case List((arg1, idx1), (arg2, idx2)) if arg1.isUsable && arg2.isUsable && f(arg1.name) =>
          Extraction(List(idx1, idx2), ExtractPair(Some(arg1.name), Some(arg2.name)))

        case List((arg1, _)) if arg1.isUsable && f(arg1.name) =>
          Extraction(List.empty, ExtractPair(Some(arg1.name), None))

      }.getOrElse(Extraction(List.empty, ExtractPair(None, None)))

      _    <- res.markUnused.traverseU(idx => markUnusable[A, E](idx))

    } yield res.extractPair
  }

  def markUnusable[A, E](idx: Int): CliParser[A, E, Unit] = {
    for {
      args    <- getArgs[A, E]
      newArgs = args.args.zipWithIndex.map {
        case (CliArgument(v, _), index) if index === idx =>
          CliArgument(v, isUsable = false)

        case (a, _) => a
      }

      _ <- setArgs(CliArguments(newArgs))

    } yield ()
  }
}
