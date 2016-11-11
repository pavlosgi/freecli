package pavlosgi.freecli.core.dsl.arguments

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.arguments._

case class ArgDslBuilder[H <: HList, T](list: H) extends Builder[H] {

  type Out[A <: HList] = ArgDslBuilder[A, T]

  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    append(description)

  def -~(
    placeholder: Placeholder)
   (implicit ev: Prepend[H, Placeholder :: HNil],
    ev2: NotContainsConstraint[H, Placeholder]) =

    append(placeholder)

  override def append[A](
   t: A)
  (implicit ev: Prepend[H, ::[A, HNil]]):
   ArgDslBuilder[ev.Out, T] = {

   new ArgDslBuilder(list :+ t)
  }
}

object ArgDslBuilder {
  def arg[T](implicit decoder: StringDecoder[T]): ArgDslBuilder[HNil, T] =
    arg(HNil)

  def arg[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new ArgDslBuilder[H, T](list)

  trait CanProduceArgDsl[F[_ <: HList, _], H <: HList, T, A] {
    def apply(a: F[H, T]): ArgDsl[A]
  }

  object CanProduceArgDsl {
    implicit def canProduceArgDsl[H <: HList, T, Out <: HList](
      implicit canProduceArgumentDetails: CanProduceArgumentDetails.Aux[T, H, HNil],
      decoder: StringDecoder[T]):
      CanProduceArgDsl[ArgDslBuilder, H, T, T] = {

      (o: ArgDslBuilder[H, T]) => {
        val (ad, _) = canProduceArgumentDetails.apply(o.list)

        new ArgDsl[T] {
          override def apply[F[_] : Algebra]: F[T] =
            implicitly[Algebra[F]].arg[T, T](ad, identity, decoder)

        }
      }
    }
  }

  implicit def toArgDsl[H <: HList, T, A](
    o: ArgDslBuilder[H, T])
   (implicit canProduceArgDsl: CanProduceArgDsl[ArgDslBuilder, H, T, A]):
    ArgDsl[A] = {

    canProduceArgDsl.apply(o)
  }

  implicit def toArgDslMerger[H <: HList, T, A](
    o: ArgDslBuilder[H, T])
   (implicit canProduceArgDsl: CanProduceArgDsl[ArgDslBuilder, H, T, A]):
    ArgDsl.Merger[A] = {

    canProduceArgDsl.apply(o)
  }

  implicit def dsl2FA[H <: HList, T, A, Out <: HList, F[_]: Algebra](
    implicit canProduceArgDsl: CanProduceArgDsl[ArgDslBuilder, H, T, A]):
    ArgDslBuilder[H, T] => F[A] = {

    canProduceArgDsl(_).apply[F]
  }
}