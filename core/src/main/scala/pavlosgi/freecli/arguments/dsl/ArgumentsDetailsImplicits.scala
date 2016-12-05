package pavlosgi.freecli.arguments.dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.Description
import pavlosgi.freecli.arguments.api.{ArgumentDetails, ArgumentName}
import pavlosgi.freecli.core.{CanProduce, Description}

trait ArgumentsDetailsImplicits {
  type ArgumentDetailsTypes = ArgumentName :: Description :: HNil

  implicit def canProduceArgumentDetails[H <: HList, Out0 <: HList, Rem <: HList](
    implicit ev: Intersection.Aux[H, ArgumentDetailsTypes, Out0],
    ev2: LeftFolder.Aux[H, ArgumentDetails, argumentDetailsBuilder.type, ArgumentDetails],
    ev3: Diff.Aux[H, Out0, Rem]) = {

    new CanProduce[H] {
      type Out = (ArgumentDetails, Rem)
      def apply(t: H): Out = {
        ev2(t, ArgumentDetails(None, None)) -> ev3(t)
      }
    }
  }

  object argumentDetailsBuilder extends Poly2 {
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
}