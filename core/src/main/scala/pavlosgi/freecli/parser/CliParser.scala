package pavlosgi.freecli.parser

import cats.{Monad, Semigroup}
import cats.data._
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core.formatting._

case class CliParser[A, E: Semigroup, T](value: EitherT[State[CliParserState, ?], EarlyTermination[A, E], T]) {
  def map[B](f: T => B): CliParser[A, E, B] = {
    CliParser(value.map(f))
  }

  def flatMap[B](f: T => CliParser[A, E, B]): CliParser[A, E, B] = {
    CliParser(value.flatMap(f(_).value))
  }

  def mapError[B: Semigroup](f: E => B): CliParser[A, B, T] = {
    CliParser(value.leftMap {
      case ErrorTermination(errors) => ErrorTermination(f(errors))
      case ActionTermination(a) => ActionTermination(a)
    })
  }

  def mapAction[Ac2](f: A => Ac2): CliParser[Ac2, E, T] = {
    CliParser(value.leftMap {
      case ActionTermination(a) => ActionTermination(f(a))
      case ErrorTermination(ers) => ErrorTermination(ers)
    })
  }

  def failIfNotAllArgumentsUsed(f: Seq[CliArgument] => E): CliParser[A, E, T] = {
    val st = for {
      res <- value.value
      state <- value.value.get
    } yield {
      val cliArgsUsable = state.args.filter(_.isUsable)
      res match {
        case Left(ErrorTermination(err)) if cliArgsUsable.nonEmpty =>
          Left[ErrorTermination[A, E], T](ErrorTermination(err |+| f(cliArgsUsable)))

        case _ if cliArgsUsable.nonEmpty =>
          Left[ErrorTermination[A, E], T](ErrorTermination(f(cliArgsUsable)))

        case r => r
      }
    }

    CliParser(EitherT[State[CliParserState, ?], EarlyTermination[A, E], T](st))
  }

  def run(args: Seq[String]): (CliParserState, Result[A, E, T]) = {
    CliParser.run[A, E, T](args)(this)
  }

  def runOrFail(
    args: Seq[String],
    failMessage: Option[String],
    actionHandler: A => Unit)
   (implicit ev: DisplayErrors[E]):
    T = {

    CliParser.runOrFail[A, E, T](args, failMessage, actionHandler)(this)
  }
}

object CliParser {
  implicit def monadInstance[A, E: Semigroup, S] = new Monad[CliParser[A, E, ?]] {
    def flatMap[T, B](fa: CliParser[A, E, T])(f: T => CliParser[A, E, B]): CliParser[A, E, B] = {
      CliParser(fa.value.flatMap(f(_).value))
    }

    def tailRecM[T, B](a: T)(f: T => CliParser[A, E, Either[T, B]]): CliParser[A, E, B] = {
      CliParser(
        implicitly[Monad[EitherT[State[CliParserState, ?], EarlyTermination[A, E], ?]]]
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
              Left(e1 |+| e2)

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

  def run[A, E: Semigroup, T](
    args: Seq[String])
   (cliParser: CliParser[A, E, T]):
    (CliParserState, Result[A, E, T]) = {

    val (st, res) = cliParser.value.value.run(
      CliParserState(
        args.map(a => CliArgument(a, isUsable = true)),
        None)).value

    res match {
      case Right(v) => (st, Success[A, E, T](v))
      case Left(ActionTermination(a)) => (st, Action[A, E, T](a))
      case Left(ErrorTermination(e)) => (st, Failure[A, E, T](e))
    }
  }

  def runOrFail[A, E: Semigroup, T](
    args: Seq[String],
    failMessage: Option[String],
    actionHandler: A => Unit)
   (cliParser: CliParser[A, E, T])
   (implicit ev: DisplayErrors[E]):
    T = {

    val (state, res) = run(args)(cliParser)
    res match {
      case Success(v)  => v
      case Action(a) =>
        actionHandler(a)
        sys.exit(1)

      case Failure(err) =>
        val errorsDisplay =
          s"""
           |${"Errors:".underline.bold.red}
           |
           |${indent(2, ev.display(err))}""".stripMargin

        val parsingDetails = state.args.map {
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
             |${state.failMessage.orElse(failMessage).getOrElse("")}
             |""".stripMargin)

        sys.exit(1)
    }
  }

  def fromState[A, E: Semigroup, T](
    s: State[CliParserState, T]):
    CliParser[A, E, T] = {

    CliParser(EitherT(s.map(Either.right)))
  }

  def fromEitherState[A, E: Semigroup, T](
    s: State[CliParserState, Either[EarlyTermination[A, E], T]]):
    CliParser[A, E, T] = {

    CliParser(EitherT(s))
  }

  def get[A, E: Semigroup]: CliParser[A, E, CliParserState] = {
    CliParser[A, E, CliParserState](EitherT(State.get[CliParserState].map(Either.right)))
  }

  def set[A, E: Semigroup](state: CliParserState): CliParser[A, E, Unit] = {
    fromState(State.set(state))
  }

  def modify[A, E: Semigroup](f: CliParserState => CliParserState): CliParser[A, E, Unit] = {
    fromState(State.modify[CliParserState](f))
  }

  def setArgs[A, E: Semigroup](args: Seq[CliArgument]): CliParser[A, E, Unit] = {
    modify(_.copy(args = args))
  }

  def getArgs[A, E: Semigroup]: CliParser[A, E, Seq[CliArgument]] = {
    get[A, E].map(_.args)
  }

  def setFailMessage[A, E: Semigroup](message: String): CliParser[A, E, Unit] = {
    modify(_.copy(failMessage = Some(message)))
  }

  def getFailMessage[A, E: Semigroup]: CliParser[A, E, Option[String]] = {
    get[A, E].map(_.failMessage)
  }

  def success[A, E: Semigroup, T](v: T): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(Right(v)))
  }

  def error[A, E: Semigroup, T](error: E): CliParser[A, E, T] = {
    errors(error)
  }

  def errors[A, E: Semigroup, T](error: E): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(Left(ErrorTermination(error))))
  }

  def action[A, E: Semigroup, T](action: A): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(Left(ActionTermination(action))))
  }

  def fromEither[A, E: Semigroup, T](v: Either[E, T]): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(v.leftMap(e => ErrorTermination[A, E](e))))
  }

  def fromValidated[A, E: Semigroup, T](v: Validated[E, T]): CliParser[A, E, T] = {
    CliParser(EitherT.fromEither(v.toEither.leftMap(e => ErrorTermination[A, E](e))))
  }

  def extractNext[A, E: Semigroup]: CliParser[A, E, Option[String]] = {
    for {
      args <- getArgs[A, E]
      res  = args.zipWithIndex.collectFirst {
        case (CliArgument(value, true), idx) =>
          Some(idx) -> Some(value)

      }.getOrElse(None -> None)

      _    <- res._1.traverseU(idx => markUnusable[A, E](idx))
    } yield res._2
  }

  def extractNextIf[A, E: Semigroup](f: String => Boolean): CliParser[A, E, Option[String]] = {
    for {
      args <- getArgs[A, E]
      res  = args.zipWithIndex.find(a => a._1.isUsable) match {
        case Some((a, idx)) if f(a.name) =>
          Some(idx) -> Some(a.name)

        case _ =>
          None -> None
      }

      _    <- res._1.traverseU(idx => markUnusable[A, E](idx))
    } yield res._2
  }

  def extract[A, E: Semigroup](f: String => Boolean): CliParser[A, E, Option[String]] = {
    for {
      args <- getArgs[A, E]
      res = args.zipWithIndex.collectFirst {
        case (CliArgument(value, true), idx) if f(value) =>
          Option(idx) -> Some(value)

      }.getOrElse(None -> None)

      _    <- res._1.traverseU(idx => markUnusable[A, E](idx))
    } yield res._2
  }

  def extractPair[A, E: Semigroup](f: String => Boolean): CliParser[A, E, ExtractPair] = {
    case class Extraction(markUnused: List[Int], extractPair: ExtractPair)

    for {
      args <- getArgs[A, E]
      res  = args.zipWithIndex.sliding(2).map(_.toList).collectFirst {
        case List((arg1, idx1), (arg2, idx2)) if arg1.isUsable && arg2.isUsable && f(arg1.name) =>
          Extraction(List(idx1, idx2), ExtractPair(Some(arg1.name), Some(arg2.name)))

        case List((arg1, _)) if arg1.isUsable && f(arg1.name) =>
          Extraction(List.empty, ExtractPair(Some(arg1.name), None))

      }.getOrElse(Extraction(List.empty, ExtractPair(None, None)))

      _    <- res.markUnused.traverseU(idx => markUnusable[A, E](idx))

    } yield res.extractPair
  }

  def markUnusable[A, E: Semigroup](idx: Int): CliParser[A, E, Unit] = {
    for {
      args    <- getArgs[A, E]
      newArgs = args.zipWithIndex.map {
        case (CliArgument(v, _), index) if index === idx =>
          CliArgument(v, isUsable = false)

        case (a, _) => a
      }

      _ <- setArgs(newArgs)

    } yield ()
  }
}
