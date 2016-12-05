package pavlosgi.freecli.core.dsl.config

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.core.dsl.toHList

class ConfigDslMerger[C](private val c: ConfigDsl[C]) {
  def ::[C2, Out <: HList](
    dsl: ConfigDslMerger[C2])
   (implicit ev: LeftFolder.Aux[C2 :: C :: HNil, HNil, toHList.type, Out]):
    ConfigDsl[Out] = {

    (dsl.c |@| c).map((l, h) => ev.apply(l :: h :: HNil, HNil))
  }
}