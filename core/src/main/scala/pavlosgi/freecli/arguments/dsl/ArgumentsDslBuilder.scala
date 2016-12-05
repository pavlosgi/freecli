package pavlosgi.freecli.arguments.dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.arguments.api._
import pavlosgi.freecli.core.{CanProduce, Description, StringDecoder}
import pavlosgi.freecli.core.{Description, StringDecoder}

case class ArgumentsDslBuilder[H <: HList, T](list: H) {
  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    new ArgumentsDslBuilder[ev.Out, T](list :+ description)

  def -~(
    name: ArgumentName)
   (implicit ev: Prepend[H, ArgumentName :: HNil],
    ev2: NotContainsConstraint[H, ArgumentName]) =

    new ArgumentsDslBuilder[ev.Out, T](list :+ name)
}

object ArgumentsDslBuilder {
  def arg[T](implicit decoder: StringDecoder[T]): ArgumentsDslBuilder[HNil, T] =
    arg(HNil)

  def arg[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new ArgumentsDslBuilder[H, T](list)

  implicit def canProduceArgDsl[H <: HList, T, Out <: HList](
    implicit canProduceArgumentDetails: CanProduce.Aux[H, (ArgumentDetails, HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[ArgumentsDslBuilder[H, T], ArgumentsDsl[T]] = {

    new CanProduce[ArgumentsDslBuilder[H, T]] {
      type Out = ArgumentsDsl[T]
      def apply(t: ArgumentsDslBuilder[H, T]): Out = {
        val (ad, _) = canProduceArgumentDetails.apply(t.list)
        FreeApplicative.lift(Arg[T, T](ad, identity, decoder))
      }
    }
  }
}