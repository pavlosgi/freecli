package freecli
package argument
package dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import api._
import core.api.{CanProduce, Description, StringDecoder}

case class ArgumentDslBuilder[H <: HList, T](list: H) {
  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    new ArgumentDslBuilder[ev.Out, T](list :+ description)

  def -~(
    name: ArgumentFieldName)
   (implicit ev: Prepend[H, ArgumentFieldName :: HNil],
    ev2: NotContainsConstraint[H, ArgumentFieldName]) =

    new ArgumentDslBuilder[ev.Out, T](list :+ name)
}

object ArgumentDslBuilder {
  def arg[T](implicit decoder: StringDecoder[T]): ArgumentDslBuilder[HNil, T] =
    arg(HNil)

  def arg[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new ArgumentDslBuilder[H, T](list)

  implicit def canProduceArgDsl[H <: HList, T, Out <: HList](
    implicit canProduceArgumentDetails: CanProduce.Aux[H, (ArgumentField, HNil)],
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
