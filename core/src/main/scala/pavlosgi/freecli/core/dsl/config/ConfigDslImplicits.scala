package pavlosgi.freecli.core.dsl.config

import shapeless.HList

import pavlosgi.freecli.core.dsl.CanProduce
import pavlosgi.freecli.core.dsl.{arguments => A, options => O}

trait ConfigDslImplicits extends O.OptionDslImplicits with A.ArgumentsDslImplicits{
  implicit def toConfigDsl[B, O](
    b: B)
   (implicit ev: CanProduce.Aux[B, ConfigDsl[O]]):
    ConfigDsl[O] = {

    ev(b)
  }

  implicit def canProduceConfigDslId[O] = CanProduce.Id[ConfigDsl[O]]
  implicit def canProduceConfigDslBuilderId[H <: HList, O, A] =
    CanProduce.Id[ConfigDslBuilder[H, O, A]]
}
