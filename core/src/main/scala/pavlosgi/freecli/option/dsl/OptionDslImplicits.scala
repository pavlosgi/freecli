package pavlosgi.freecli.option.dsl

import pavlosgi.freecli.core.CanProduce

trait OptionDslImplicits extends DefaultImplicits with FieldImplicits with DescriptionImplicits with MergerImplicits {

  implicit def toOptionDsl[B, O](
    b: B)
   (implicit ev: CanProduce.Aux[B, OptionDsl[O]]):
    OptionDsl[O] = {

    ev(b)
  }

  implicit def canProduceOptionDslId[O] = CanProduce.Id[OptionDsl[O]]
}