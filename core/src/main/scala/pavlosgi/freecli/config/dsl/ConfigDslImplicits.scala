package pavlosgi.freecli.config.dsl

import shapeless.HList

import pavlosgi.freecli.arguments.{dsl => A}
import pavlosgi.freecli.core.CanProduce
import pavlosgi.freecli.options.{dsl => O}

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
