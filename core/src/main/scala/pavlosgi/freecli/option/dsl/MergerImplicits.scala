package pavlosgi.freecli.option.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.core.{CanProduce, Merger, toHList}
import pavlosgi.freecli.core.Merger.CanMerge

trait MergerImplicits {
  implicit def options2Merger[B, O](
    b: B)(
    implicit ev: CanProduce.Aux[B, OptionDsl[O]]): Merger[OptionDsl[O]] = {

    Merger[OptionDsl[O]](ev(b))
  }

  implicit def canMergeOptions[O1, O2, O <: HList](
    implicit folder: LeftFolder.Aux[O1 :: O2 :: HNil, HNil, toHList.type, O]) = {

    new CanMerge[OptionDsl[O1], OptionDsl[O2]] {
      type Out = OptionDsl[O]
      def apply(f1: OptionDsl[O1], f2: OptionDsl[O2]): Out = {
        (f1 |@| f2).map((ff1, ff2) => folder(ff1 :: ff2 :: HNil, HNil))
      }
    }
  }
}
