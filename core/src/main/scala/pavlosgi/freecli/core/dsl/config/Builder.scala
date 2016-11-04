package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

trait Builder[H <: HList] {
  type Out[T <: HList]
  def append[A](t: A)(implicit ev: Prepend[H, A :: HNil]): Out[ev.Out]
}

object Builder {
  type Aux[H <: HList, Out_[_]] = Builder[H] {type Out[A] = Out_[A]}
}