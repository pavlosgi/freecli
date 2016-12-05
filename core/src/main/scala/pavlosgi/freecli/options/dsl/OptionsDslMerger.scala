package pavlosgi.freecli.options.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.core.toHList

class OptionsDslMerger[H](private val c: OptionsDsl[H]) {
  def ::[H2, Out <: HList](
    merger: OptionsDslMerger[H2])
   (implicit ev: LeftFolder.Aux[H2 :: H :: HNil, HNil, toHList.type, Out]):
    OptionsDsl[Out] = {

    (merger.c |@| c).map((l, h) => ev.apply(l :: h :: HNil, HNil))
  }
}