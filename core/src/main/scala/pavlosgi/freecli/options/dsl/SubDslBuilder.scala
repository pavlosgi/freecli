package pavlosgi.freecli.options.dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.CanProduce
import pavlosgi.freecli.core.Description
import pavlosgi.freecli.options.api.Sub

case class SubDslBuilder[H <: HList, T](list: H) {

  def apply[Conf](
    f: OptionsDsl[Conf])
  (implicit ev: NotContainsConstraint[H, OptionsDsl[_]],
    ev2: Prepend[H, OptionsDsl[Conf] :: HNil]) = {

    new SubDslBuilder[ev2.Out, Conf](list :+ f)
  }
}

object SubDslBuilder {
  def sub(description: Description) = new SubDslBuilder(description :: HNil)

  implicit def canProduceConfigDsl[H <: HList, T]
   (implicit canProduceDescription: CanProduce.Aux[H, (Description, OptionsDsl[T] :: HNil)]):
    CanProduce.Aux[SubDslBuilder[H, T], OptionsDsl[T]] = {

    new CanProduce[SubDslBuilder[H, T]] {
      type Out = OptionsDsl[T]
      def apply(v: SubDslBuilder[H, T]): Out = {
        val (description, remaining) = canProduceDescription.apply(v.list)
        val subDsl = remaining.head
        FreeApplicative.lift(Sub[T](description, subDsl))
      }
    }
  }
}