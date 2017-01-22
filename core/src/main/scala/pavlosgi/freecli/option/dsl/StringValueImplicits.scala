package pavlosgi.freecli.option.dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.api.CanProduce
import pavlosgi.freecli.option.api.StringValue

trait StringValueImplicits {
  type StringValueTypes = StringValue :: HNil

  implicit def canProduceStringValue[T, H <: HList, Out0 <: HList, Rem <: HList](
    implicit ev: Intersection.Aux[H, StringValueTypes, Out0],
    ev2: LeftFolder.Aux[Out0, Option[StringValue], stringValueBuilder.type, StringValue],
    ev3: Diff.Aux[H, Out0, Rem]) = {

    new CanProduce[H] {
      type Out = (StringValue, Rem)

      def apply(list: H): Out = {
        val inters = ev.apply(list)
        val field = inters.foldLeft(Option.empty[StringValue])(stringValueBuilder)
        val remaining = ev3.apply(list)

        field -> remaining
      }
    }
  }

  object stringValueBuilder extends Poly2 {
    implicit def caseStringValue:
      Case.Aux[Option[StringValue], StringValue, StringValue] =

      at[Option[StringValue], StringValue] {
        case (_, d: StringValue) => d
      }
  }
}