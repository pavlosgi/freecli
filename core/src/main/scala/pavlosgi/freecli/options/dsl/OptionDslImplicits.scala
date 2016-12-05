package pavlosgi.freecli.options.dsl

import pavlosgi.freecli.core.CanProduce

trait OptionDslImplicits extends DefaultImplicits with FieldImplicits with DescriptionImplicits {
  implicit def toOptionsDslMerger[B, O](
    b: B)
   (implicit ev: CanProduce.Aux[B, OptionsDsl[O]]):
    OptionsDslMerger[O] = {

    new OptionsDslMerger[O](ev(b))
  }

  implicit def toOptionsDsl[B, O](
    b: B)
   (implicit ev: CanProduce.Aux[B, OptionsDsl[O]]):
    OptionsDsl[O] = {

    ev(b)
  }

  implicit def canProduceOptionsDslId[O] = CanProduce.Id[OptionsDsl[O]]
}