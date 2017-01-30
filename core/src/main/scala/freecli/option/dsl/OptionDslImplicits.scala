package freecli
package option
package dsl

import core.api.CanProduce

trait OptionDslImplicits {
  implicit def toOptionDsl[B, O](
    b: B)
   (implicit ev: CanProduce.Aux[B, OptionDsl[O]]):
    OptionDsl[O] = {

    ev(b)
  }

  implicit def canProduceOptionDslId[O] = CanProduce.Id[OptionDsl[O]]
}