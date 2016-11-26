package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._

private[config] sealed trait CanProduceArgumentDetails[T, H <: HList] {
  type Out <: HList

  def apply(list: H): (ArgumentDetails, Out)
}

private[config] object CanProduceArgumentDetails {
  type ArgumentDetailsTypes =
    ArgumentName :: Description :: HNil

  type Aux[T, H <: HList, Out_ <: HList] = CanProduceArgumentDetails[T, H] {
    type Out = Out_
  }

  object aggregate extends Poly2 {
    implicit def caseArgumentDetailsPlaceholder:
      Case.Aux[ArgumentDetails, ArgumentName, ArgumentDetails] =

      at[ArgumentDetails, ArgumentName] {
        case (ad, p: ArgumentName) =>
          ArgumentDetails(Some(p), ad.description)
      }

    implicit def caseArgumentDetailsDescription:
      Case.Aux[ArgumentDetails, Description, ArgumentDetails] =

      at[ArgumentDetails, Description] {
        case (ad, d) =>
          ad.copy(description = Some(d))
      }
  }

  implicit def canProduceArgumentDetails[T, H <: HList, Out0 <: HList, Out1 <: HList](
    implicit ev: Intersection.Aux[H, ArgumentDetailsTypes, Out0],
    ev2: LeftFolder.Aux[Out0, ArgumentDetails, aggregate.type, ArgumentDetails],
    ev3: Diff.Aux[H, Out0, Out1]) = {

    new CanProduceArgumentDetails[T, H] {
      type Out = Out1

      def apply(list: H): (ArgumentDetails, Out) = {
        val inters = ev.apply(list)
        val field = inters.foldLeft(ArgumentDetails(None, None))(aggregate)
        val remaining = ev3.apply(list)

        field -> remaining
      }
    }
  }
}