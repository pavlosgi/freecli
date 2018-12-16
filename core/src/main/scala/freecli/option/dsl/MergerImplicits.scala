package freecli
package option
package dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.LeftFolder

import core.api.{CanProduce, Merger}
import core.api.CanMerge
import core.poly.toHList

trait MergerImplicits {
  implicit def options2Merger[B, O](
    b: B)(
    implicit ev: CanProduce.Aux[B, OptionDsl[O]]):
    Merger[OptionDsl[O]] = {

    Merger[OptionDsl[O]](ev(b))
  }

  implicit def canMergeOptions[O1, O2, O <: HList](
    implicit folder: LeftFolder.Aux[O1 :: O2 :: HNil, HNil, toHList.type, O]):
    CanMerge.Aux[OptionDsl[O1], OptionDsl[O2], OptionDsl[O]] = {

    new CanMerge[OptionDsl[O1], OptionDsl[O2]] {
      type Out = OptionDsl[O]
      def apply(f1: OptionDsl[O1], f2: OptionDsl[O2]): Out = {
        (f1, f2).mapN((ff1, ff2) => folder(ff1 :: ff2 :: HNil, HNil))
      }
    }
  }
}
