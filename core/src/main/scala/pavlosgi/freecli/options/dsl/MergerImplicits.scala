package pavlosgi.freecli.options.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.core.{CanProduce, Merger, toHList}
import pavlosgi.freecli.core.Merger.CanMerge

trait MergerImplicits {
  implicit def options2Merger[B, O](
    b: B)(
    implicit ev: CanProduce.Aux[B, OptionsDsl[O]]): Merger[OptionsDsl[O]] = {

    Merger[OptionsDsl[O]](ev(b))
  }

  implicit def canMergeOptions[O1, O2, O <: HList](
    implicit folder: LeftFolder.Aux[O1 :: O2 :: HNil, HNil, toHList.type, O]) = {

    new CanMerge[OptionsDsl[O1], OptionsDsl[O2]] {
      type Out = OptionsDsl[O]
      def apply(f1: OptionsDsl[O1], f2: OptionsDsl[O2]): Out = {
        (f1 |@| f2).map((ff1, ff2) => folder(ff1 :: ff2 :: HNil, HNil))
      }
    }
  }
}
