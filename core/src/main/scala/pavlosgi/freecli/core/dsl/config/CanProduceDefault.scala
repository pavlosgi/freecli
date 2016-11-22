package pavlosgi.freecli.core.dsl.config

import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}
import shapeless.{::, HList, HNil, Poly2}

import pavlosgi.freecli.core.dsl.config.OptDslBuilder.DefaultValue

trait CanProduceDefault[T, H <: HList] {
  type Out <: HList
  def apply(list: H): (T, Out)
}

object CanProduceDefault {

  type DefaultTypes[T] = DefaultValue[T] :: HNil

  type Aux[T, H <: HList, Out_ <: HList] = CanProduceDefault[T, H] {
    type Out = Out_
  }

  object aggregate extends Poly2 {
    implicit def caseOptionDefault[T]:
      Case.Aux[Option[T], DefaultValue[T], T] =

      at[Option[T], DefaultValue[T]] {
        case (_, default) => default.value
      }
  }

  implicit def canProduceDefault[T, H <: HList, Out0 <: HList, Out1 <: HList](
    implicit ev: Intersection.Aux[H, DefaultTypes[T], Out0],
    ev2: LeftFolder.Aux[Out0, Option[T], aggregate.type, T],
    ev3: Diff.Aux[H, Out0, Out1]) = {

    new CanProduceDefault[T, H] {
      type Out = Out1

      def apply(list: H): (T, Out) = {
        val inters = ev.apply(list)
        val default = inters.foldLeft(Option.empty[T])(aggregate)
        val remaining = ev3.apply(list)

        default -> remaining
      }
    }
  }
}
