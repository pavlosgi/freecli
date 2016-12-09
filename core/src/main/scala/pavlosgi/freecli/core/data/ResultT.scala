package pavlosgi.freecli.core.data

import cats.Monad
import cats.data.{NonEmptyList, _}
import cats.syntax.all._

case class ResultT[E, S, A](value: EitherT[State[S, ?], NonEmptyList[E], A]) {
  def map[B](f: A => B): ResultT[E, S, B] = {
    ResultT(value.map(f))
  }

  def flatMap[B](f: A => ResultT[E, S, B]): ResultT[E, S, B] = {
    ResultT(value.flatMap(f(_).value))
  }

  def leftMapInner[B](f: E => B): ResultT[B, S, A] = {
    ResultT(value.leftMap(_.map(f)))
  }

  def leftMap[B](f: NonEmptyList[E] => NonEmptyList[B]): ResultT[B, S, A] = {
    ResultT(value.leftMap(f))
  }
}

object ResultT {
  implicit def monadInstance[E, S] = new Monad[ResultT[E, S, ?]] {
    def flatMap[A, B](fa: ResultT[E, S, A])(f: A => ResultT[E, S, B]): ResultT[E, S, B] = {
      ResultT(fa.value.flatMap(f(_).value))
    }

    def tailRecM[A, B](a: A)(f: A => ResultT[E, S, Either[A, B]]): ResultT[E, S, B] = {
      ResultT(
        implicitly[Monad[EitherT[State[S, ?], NonEmptyList[E], ?]]]
          .tailRecM(a)(a => f(a).value))
    }

    def pure[A](x: A): ResultT[E, S, A] = ResultT(EitherT.pure(x))

    override def ap[A, B](ff: ResultT[E, S, A => B])(fa: ResultT[E, S, A]): ResultT[E, S, B] = {
      ResultT(EitherT(
        fa.value.value.flatMap { ei =>
          ff.value.withValidated(f => Validated.fromEither(ei).ap(f)).value
        }))
    }

    override def product[A, B](fa: ResultT[E, S, A], fb: ResultT[E, S, B]): ResultT[E, S, (A, B)] = {
      ResultT.fromState(
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

  def run[E, S, A]
    (state: S)
    (resultT: ResultT[E, S, A]):
    (S, Either[NonEmptyList[E], A]) = {

    resultT.value.value.run(state).value
  }

  def runA[E, S, A]
    (state: S)
    (resultT: ResultT[E, S, A]):
    Either[NonEmptyList[E], A] = {

    resultT.value.value.runA(state).value
  }

  def right[E, S, A](v: A): ResultT[E, S, A] = {
    ResultT(EitherT.fromEither[State[S, ?]](Either.right(v)))
  }

  def leftNE[E, S, A](v: E): ResultT[E, S, A] = {
    ResultT(EitherT.fromEither[State[S, ?]](Either.left(NonEmptyList.of(v))))
  }

  def get[E, S]: ResultT[E, S, S] = {
    ResultT(EitherT.apply(State.get[S].map(Either.right)))
  }

  def set[E, S](state: S): ResultT[E, S, Unit] = {
    ResultT(EitherT.apply(State.set[S](state).map(Either.right)))
  }

  def pure[E, S, A](v: A): ResultT[E, S, A] = {
    ResultT(EitherT.pure[State[S, ?], NonEmptyList[E], A](v))
  }

  def fromEither[E, S, A](v: Either[NonEmptyList[E], A]): ResultT[E, S, A] = {
    ResultT(EitherT.fromEither[State[S, ?]](v))
  }

  def fromValidated[E, S, A](v: ValidatedNel[E, A]): ResultT[E, S, A] = {
    fromEither(v.toEither)
  }

  def fromOption[E, S, A](v: Option[A], e: E): ResultT[E, S, A] = {
    ResultT(EitherT.fromEither[State[S, ?]](v.toRight(NonEmptyList.of(e))))
  }

  def fromState[E, S, A](s: State[S, Either[NonEmptyList[E], A]]): ResultT[E, S, A] = {
    ResultT(EitherT(s))
  }
}