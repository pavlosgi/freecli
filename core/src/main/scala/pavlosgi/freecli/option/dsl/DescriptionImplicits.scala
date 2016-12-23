package pavlosgi.freecli.option.dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.Description
import pavlosgi.freecli.core.Description
import pavlosgi.freecli.core.ops.CanProduce

trait DescriptionImplicits {
  type DescriptionTypes = Description :: HNil

  implicit def canProduceDescription[T, H <: HList, Out0 <: HList, Rem <: HList](
    implicit ev: Intersection.Aux[H, DescriptionTypes, Out0],
    ev2: LeftFolder.Aux[Out0, Option[Description], descriptionBuilder.type, Description],
    ev3: Diff.Aux[H, Out0, Rem]) = {

    new CanProduce[H] {
      type Out = (Description, Rem)

      def apply(list: H): Out = {
        val inters = ev.apply(list)
        val field = inters.foldLeft(Option.empty[Description])(descriptionBuilder)
        val remaining = ev3.apply(list)

        field -> remaining
      }
    }
  }

  object descriptionBuilder extends Poly2 {
    implicit def caseDescription:
      Case.Aux[Option[Description], Description, Description] =

      at[Option[Description], Description] {
        case (_, d: Description) => d
      }
  }
}