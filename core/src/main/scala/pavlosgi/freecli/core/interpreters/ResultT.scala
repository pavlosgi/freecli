package pavlosgi.freecli.core.interpreters

import cats.data._

object ResultT {
  def run[E, A]
    (args: Arguments)
    (resultT: ResultT[E, A]):
    (Arguments, Either[NonEmptyList[E], A]) = {

    resultT.value.run(args).value
  }

  def runA[E, A]
    (args: Arguments)
    (resultT: ResultT[E, A]):
    Either[NonEmptyList[E], A] = {

    resultT.value.runA(args).value
  }

  def right[E, A](v: A): ResultT[E, A] = {
    EitherT.fromEither[State[Arguments, ?]](Right(v))
  }

  def leftNE[E, A](v: E): ResultT[E, A] = {
    EitherT.fromEither[State[Arguments, ?]](Left(NonEmptyList.of(v)))
  }

  def get[E]: ResultT[E, Arguments] = {
    EitherT.apply(State.get[Arguments].map(Right.apply))
  }

  def set[E](args: Arguments): ResultT[E, Unit] = {
    EitherT.apply(State.set[Arguments](args).map(Right.apply))
  }

  def pure[E, A](v: A): ResultT[E, A] = {
    EitherT.pure[State[Arguments, ?], NonEmptyList[E], A](v)
  }

  def fromEither[E, A](v: Either[NonEmptyList[E], A]): ResultT[E, A] = {
    EitherT.fromEither[State[Arguments, ?]](v)
  }

  def fromValidated[E, A](v: ValidatedNel[E, A]): ResultT[E, A] = {
    fromEither(v.toEither)
  }

  def fromOption[E, A](v: Option[A], e: E): ResultT[E, A] = {
    EitherT.fromEither[State[Arguments, ?]](v.toRight(NonEmptyList.of(e)))
  }
}
