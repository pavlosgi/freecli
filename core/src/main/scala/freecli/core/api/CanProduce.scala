package freecli
package core
package api

import shapeless._

trait CanProduce[T] extends DepFn1[T]
object CanProduce {
  type Aux[T, Out0] = CanProduce[T] { type Out = Out0 }

  def Id[T] = new CanProduce[T] {
    type Out = T
    def apply(t: T): Out = t
  }
}
