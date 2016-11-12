package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.api.config.Description
import pavlosgi.freecli.core.dsl.config.CanProduceDescription._

trait CanProduceDescription[H <: HList] {
  type IOut <: HList
  type DOut <: HList

  val intersection: Intersection.Aux[H, DescriptionTypes, IOut]
  val folder: LeftFolder.Aux[IOut, Option[Description], aggregate.type, Description]
  val diff: Diff.Aux[H, IOut, DOut]

  def apply(list: H): (Description, DOut) = {
    val inters = intersection.apply(list)
    val field = getDescription(inters)(folder)
    val remaining = diff.apply(list)

    field -> remaining
  }
}

object CanProduceDescription {
  type DescriptionTypes = Description :: HNil

  type Aux[H <: HList, Out <: HList] = CanProduceDescription[H] {
    type DOut = Out
  }

  implicit def canProduceDescription[T, H <: HList, Out0 <: HList, Out1 <: HList](
    implicit ev: Intersection.Aux[H, DescriptionTypes, Out0],
    ev2: LeftFolder.Aux[Out0, Option[Description], aggregate.type, Description],
    ev3: Diff.Aux[H, Out0, Out1]) = {

    new CanProduceDescription[H] {
      override type IOut = Out0
      override type DOut = Out1
      override val intersection = ev
      override val folder = ev2
      override val diff = ev3
    }

  }

  def getDescription[E <: HList](
    list: E)
   (implicit ev: LeftFolder.Aux[E, Option[Description], aggregate.type, Description]):
    ev.Out = {

    list.foldLeft(Option.empty[Description])(aggregate)
  }

  object aggregate extends Poly2 {
    implicit def caseDescription:
      Case.Aux[Option[Description], Description, Description] =

      at[Option[Description], Description] {
        case (_, d: Description) => d
      }
  }
}