package pavlosgi.freecli.core.dsl.arguments

import cats.syntax.all._

import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.core.dsl.toHList

class ArgumentDslMerger[A](private val c: ArgumentsDsl[A]) {
  def ::[A2, Out <: HList](
    dsl: ArgumentDslMerger[A2])
   (implicit ev: LeftFolder.Aux[A2 :: A :: HNil, HNil, toHList.type, Out]):
    ArgumentsDsl[Out] = {

    (dsl.c |@| c).map((l, h) => ev.apply(l :: h :: HNil, HNil))
  }
}