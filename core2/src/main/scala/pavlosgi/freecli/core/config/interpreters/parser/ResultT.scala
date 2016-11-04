package pavlosgi.freecli.core.config.interpreters.parser

import cats.data._
import cats.syntax.all._

object ResultT {
  def run[E, A]
    (args: Arguments)
    (resultT: ResultT[E, A]):
    (Arguments, Xor[NonEmptyList[E], A]) = {

    resultT.value.run(args).value
  }

  def right[E, A](v: A): ResultT[E, A] = {
    XorT.fromXor[State[Arguments, ?]](Xor.right(v))
  }

  def leftNE[E, A](v: E): ResultT[E, A] = {
    XorT.fromXor[State[Arguments, ?]](Xor.left(NonEmptyList(v)))
  }

  def get[E]: ResultT[E, Arguments] = {
    XorT.apply(State.get[Arguments].map(Xor.right))
  }

  def set[E](args: Arguments): ResultT[E, Unit] = {
    XorT.apply(State.set[Arguments](args).map(Xor.right))
  }

  def pure[E, A](v: A): ResultT[E, A] = {
    XorT.pure[State[Arguments, ?], NonEmptyList[E], A](v)
  }

  def fromXor[E, A](v: Xor[NonEmptyList[E], A]): ResultT[E, A] = {
    XorT.fromXor[State[Arguments, ?]](v)
  }

  def fromValidated[E, A](v: ValidatedNel[E, A]): ResultT[E, A] = {
    fromXor(v.toXor)
  }

  def fromOption[E, A](v: Option[A], e: E): ResultT[E, A] = {
    XorT.fromXor[State[Arguments, ?]](v.toRightXor(NonEmptyList(e)))
  }
}
