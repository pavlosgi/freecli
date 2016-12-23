package pavlosgi.freecli.option.dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.ops.CanProduce
import pavlosgi.freecli.option.dsl.OptDslBuilder.DefaultValue

trait DefaultImplicits {
  type DefaultTypes[T] = DefaultValue[T] :: HNil

  implicit def canProduceDefault[H <: HList, T, Out0 <: HList, Rem <: HList](
    implicit ev: Intersection.Aux[H, DefaultTypes[T], Out0],
    ev2: LeftFolder.Aux[Out0, Option[T], defaultBuilder.type, DefaultValue[T]],
    ev3: Diff.Aux[H, Out0, Rem]) = {

    new CanProduce[H] {
      type Out = (DefaultValue[T], Rem)

      def apply(list: H): Out = {
        val inters = ev.apply(list)
        val default = inters.foldLeft(Option.empty[T])(defaultBuilder)
        val remaining = ev3.apply(list)

        default -> remaining
      }
    }
  }

  object defaultBuilder extends Poly2 {
    implicit def caseOptionDefault[T]:
      Case.Aux[Option[T], DefaultValue[T], DefaultValue[T]] =

      at[Option[T], DefaultValue[T]] {
        case (_, default) => default
      }
  }
}
