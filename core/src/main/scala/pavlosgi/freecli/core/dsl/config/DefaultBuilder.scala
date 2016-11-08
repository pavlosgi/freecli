package pavlosgi.freecli.core.dsl.config

import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}
import shapeless.{::, HList, HNil, Poly2}

case class DefaultValue[T](value: T)

object DefaultBuilder {
  type DefaultTypes[T] = DefaultValue[T] :: HNil

  abstract class CanProduceDefault[T, H <: HList] {
    type IOut <: HList
    type DOut <: HList

    val intersection: Intersection.Aux[H, DefaultTypes[T], IOut]
    val folder: LeftFolder.Aux[IOut, Option[T], aggregate.type, T]
    val diff: Diff.Aux[H, IOut, DOut]

    def apply(list: H): (T, DOut) = {
      val inters = intersection.apply(list)
      val default = getDefault(inters)(folder)
      val remaining = diff.apply(list)

      default -> remaining
    }
  }

  object CanProduceDefault {
    type Aux[T, H <: HList, Out <: HList] = CanProduceDefault[T, H] {
      type DOut = Out
    }

    implicit def canProduceDefault[T, H <: HList, Out0 <: HList, Out1 <: HList](
      implicit ev: Intersection.Aux[H, DefaultTypes[T], Out0],
      ev2: LeftFolder.Aux[Out0, Option[T], aggregate.type, T],
      ev3: Diff.Aux[H, Out0, Out1]) = {

      new CanProduceDefault[T, H] {
        type IOut = Out0
        type DOut = Out1
        val intersection = ev
        val folder = ev2
        val diff = ev3
      }

    }
  }

  def getDefault[E <: HList, T](
    list: E)
   (implicit ev: LeftFolder.Aux[E, Option[T], aggregate.type, T]):
    ev.Out = {

    list.foldLeft(Option.empty[T])(aggregate)
  }

  object aggregate extends Poly2 {
    implicit def caseOptionDefault[T]:
      Case.Aux[Option[T], DefaultValue[T], T] =

      at[Option[T], DefaultValue[T]] {
        case (_, default) => default.value
      }
  }
}
