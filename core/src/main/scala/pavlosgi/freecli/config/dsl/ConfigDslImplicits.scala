package pavlosgi.freecli.config.dsl

import cats.free.FreeApplicative

import pavlosgi.freecli.arguments.{dsl => A}
import pavlosgi.freecli.config.api.{Args, Opts}
import pavlosgi.freecli.core.CanProduce
import pavlosgi.freecli.options.{dsl => O}

trait ConfigDslImplicits extends O.OptionDslImplicits with A.ArgumentsDslImplicits with MergerImplicits {
  implicit def toConfigDsl[B, C](
    b: B)
   (implicit ev: CanProduce.Aux[B, ConfigDsl[C]]):
    ConfigDsl[C] = {

    ev(b)
  }

  implicit def optionsDsl2ConfigDsl[B, O](
    b: B)
   (implicit ev: CanProduce.Aux[B, O.OptionsDsl[O]]):
    ConfigDsl[O] = {

    FreeApplicative.lift(Opts(ev(b)))
  }

  implicit def argumentsDsl2ConfigDsl[B, A](
    b: B)
   (implicit ev: CanProduce.Aux[B, A.ArgumentsDsl[A]]): ConfigDsl[A] = {
    FreeApplicative.lift(Args(ev(b)))
  }

  implicit def canProduceConfigDslId[O] = CanProduce.Id[ConfigDsl[O]]
}
