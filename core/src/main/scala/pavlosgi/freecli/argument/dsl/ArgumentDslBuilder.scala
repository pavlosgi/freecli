package pavlosgi.freecli.argument.dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.argument.api._
import pavlosgi.freecli.core.{CanProduce, Description, StringDecoder}

case class ArgumentDslBuilder[H <: HList, T](list: H) {
  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    new ArgumentDslBuilder[ev.Out, T](list :+ description)

  def -~(
    name: ArgumentName)
   (implicit ev: Prepend[H, ArgumentName :: HNil],
    ev2: NotContainsConstraint[H, ArgumentName]) =

    new ArgumentDslBuilder[ev.Out, T](list :+ name)
}

object ArgumentDslBuilder {
  def arg[T](implicit decoder: StringDecoder[T]): ArgumentDslBuilder[HNil, T] =
    arg(HNil)

  def arg[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new ArgumentDslBuilder[H, T](list)

  implicit def canProduceArgDsl[H <: HList, T, Out <: HList](
    implicit canProduceArgumentDetails: CanProduce.Aux[H, (ArgumentDetails, HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[ArgumentDslBuilder[H, T], ArgumentDsl[T]] = {

    new CanProduce[ArgumentDslBuilder[H, T]] {
      type Out = ArgumentDsl[T]
      def apply(t: ArgumentDslBuilder[H, T]): Out = {
        val (ad, _) = canProduceArgumentDetails.apply(t.list)
        FreeApplicative.lift(Arg[T, T](ad, identity, decoder))
      }
    }
  }
}