package pavlosgi.freecli.core

import cats.data._
import cats.syntax.all._

object ResultTS {
  def run[E, S, A]
    (state: S)
    (resultT: ResultTS[E, S, A]):
    (S, Either[NonEmptyList[E], A]) = {

    resultT.value.run(state).value
  }

  def runA[E, S, A]
    (state: S)
    (resultT: ResultTS[E, S, A]):
    Either[NonEmptyList[E], A] = {

    resultT.value.runA(state).value
  }

  def right[E, S, A](v: A): ResultTS[E, S, A] = {
    EitherT.fromEither[State[S, ?]](Either.right(v))
  }

  def leftNE[E, S, A](v: E): ResultTS[E, S, A] = {
    EitherT.fromEither[State[S, ?]](Either.left(NonEmptyList.of(v)))
  }

  def get[E, S]: ResultTS[E, S, S] = {
    EitherT.apply(State.get[S].map(Either.right))
  }

  def set[E, S](state: S): ResultTS[E, S, Unit] = {
    EitherT.apply(State.set[S](state).map(Either.right))
  }

  def pure[E, S, A](v: A): ResultTS[E, S, A] = {
    EitherT.pure[State[S, ?], NonEmptyList[E], A](v)
  }

  def fromEither[E, S, A](v: Either[NonEmptyList[E], A]): ResultTS[E, S, A] = {
    EitherT.fromEither[State[S, ?]](v)
  }

  def fromValidated[E, S, A](v: ValidatedNel[E, A]): ResultTS[E, S, A] = {
    fromEither(v.toEither)
  }

  def fromOption[E, S, A](v: Option[A], e: E): ResultTS[E, S, A] = {
    EitherT.fromEither[State[S, ?]](v.toRight(NonEmptyList.of(e)))
  }
}
