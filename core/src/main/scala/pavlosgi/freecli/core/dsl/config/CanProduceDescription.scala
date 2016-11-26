package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.api.Description

private[config] sealed trait CanProduceDescription[H <: HList] {
  type Out <: HList
  def apply(list: H): (Description, Out)
}

private[config] object CanProduceDescription {
  type DescriptionTypes = Description :: HNil

  type Aux[H <: HList, Out_ <: HList] = CanProduceDescription[H] {
    type Out = Out_
  }

  implicit def canProduceDescription[T, H <: HList, Out0 <: HList, Out1 <: HList](
    implicit ev: Intersection.Aux[H, DescriptionTypes, Out0],
    ev2: LeftFolder.Aux[Out0, Option[Description], aggregate.type, Description],
    ev3: Diff.Aux[H, Out0, Out1]) = {

    new CanProduceDescription[H] {
      type Out = Out1

      def apply(list: H): (Description, Out) = {
        val inters = ev.apply(list)
        val field = inters.foldLeft(Option.empty[Description])(aggregate)
        val remaining = ev3.apply(list)

        field -> remaining
      }
    }
  }

  object aggregate extends Poly2 {
    implicit def caseDescription:
      Case.Aux[Option[Description], Description, Description] =

      at[Option[Description], Description] {
        case (_, d: Description) => d
      }
  }
}