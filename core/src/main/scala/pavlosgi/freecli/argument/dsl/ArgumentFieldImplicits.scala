package pavlosgi.freecli.argument.dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.argument.api.{ArgumentField, ArgumentFieldName}
import pavlosgi.freecli.core.Description
import pavlosgi.freecli.core.ops.CanProduce

trait ArgumentFieldImplicits {
  type ArgumentFieldTypes = ArgumentFieldName :: Description :: HNil

  implicit def canProduceArgumentField[H <: HList, Out0 <: HList, Rem <: HList](
    implicit ev: Intersection.Aux[H, ArgumentFieldTypes, Out0],
    ev2: LeftFolder.Aux[H, ArgumentField, argumentDetailsBuilder.type, ArgumentField],
    ev3: Diff.Aux[H, Out0, Rem]) = {

    new CanProduce[H] {
      type Out = (ArgumentField, Rem)
      def apply(t: H): Out = {
        ev2(t, ArgumentField(None, None)) -> ev3(t)
      }
    }
  }

  object argumentDetailsBuilder extends Poly2 {
    implicit def caseArgumentFieldArgumentFieldName:
      Case.Aux[ArgumentField, ArgumentFieldName, ArgumentField] =

      at[ArgumentField, ArgumentFieldName] {
        case (ad, p: ArgumentFieldName) =>
          ArgumentField(Some(p), ad.description)
      }

    implicit def caseArgumentFieldDescription:
      Case.Aux[ArgumentField, Description, ArgumentField] =

      at[ArgumentField, Description] {
        case (ad, d) =>
          ad.copy(description = Some(d))
      }
  }
}