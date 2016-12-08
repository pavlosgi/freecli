package pavlosgi.freecli.options.dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.{LeftFolder, Prepend, Tupler}

import pavlosgi.freecli.core.{CanProduce, Description, generic}
import pavlosgi.freecli.options.api.Sub

case class SubDslBuilder[H <: HList, T](list: H)

case class SubDslBuilderApply[H <: HList, T](list: H) {
  def apply[Conf](
    dsl: OptionsDsl[Conf])
  (implicit ev: NotContainsConstraint[H, OptionsDsl[_]],
    ev2: LeftFolder.Aux[Conf :: HNil, Option[T], generic.type, T],
    ev3: Prepend[H, OptionsDsl[T] :: HNil]) = {

    new SubDslBuilder[ev3.Out, T](list :+ dsl.map(d => ev2(d :: HNil, None)))
  }
}

case class SubDslBuilderTupler[H <: HList](list: H) {
  def apply[Conf <: HList, T](
    dsl: OptionsDsl[Conf])
  (implicit ev: NotContainsConstraint[H, OptionsDsl[_]],
    ev2: Tupler.Aux[Conf, T],
    ev3: Prepend[H, OptionsDsl[T] :: HNil]) = {

    new SubDslBuilder[ev3.Out, T](list :+ dsl.map(ev2.apply))
  }
}

object SubDslBuilder {
  def sub[T](description: Description) = {
    new SubDslBuilderApply[Description :: HNil, T](description :: HNil)
  }

  def subT(description: Description) = {
    new SubDslBuilderTupler[Description :: HNil](description :: HNil)
  }

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