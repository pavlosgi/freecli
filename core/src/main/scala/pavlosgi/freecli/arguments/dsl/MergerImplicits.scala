package pavlosgi.freecli.arguments.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.core.Merger.CanMerge
import pavlosgi.freecli.core.{CanProduce, Merger, toHList}

trait MergerImplicits {
  implicit def arguments2Merger[B, O](
    b: B)(
    implicit ev: CanProduce.Aux[B, ArgumentsDsl[O]]): Merger[ArgumentsDsl[O]] = {

    Merger[ArgumentsDsl[O]](ev(b))
  }

  implicit def canMergeArguments[A1, A2, A <: HList](
    implicit folder: LeftFolder.Aux[A1 :: A2 :: HNil, HNil, toHList.type, A]) = {

    new CanMerge[ArgumentsDsl[A1], ArgumentsDsl[A2]] {
      type Out = ArgumentsDsl[A]
      def apply(f1: ArgumentsDsl[A1], f2: ArgumentsDsl[A2]): Out = {
        (f1 |@| f2).map((ff1, ff2) => folder(ff1 :: ff2 :: HNil, HNil))
      }
    }
  }
}
