package pavlosgi.freecli.config.dsl

import cats.free.FreeApplicative

import pavlosgi.freecli.argument.{dsl => A}
import pavlosgi.freecli.config.api.{Args, Opts}
import pavlosgi.freecli.core.CanProduce
import pavlosgi.freecli.option.{dsl => O}

trait ConfigDslImplicits extends O.OptionDslImplicits with A.ArgumentDslImplicits with MergerImplicits {
  implicit def toConfigDsl[B, C](
    b: B)
   (implicit ev: CanProduce.Aux[B, ConfigDsl[C]]):
    ConfigDsl[C] = {

    ev(b)
  }

  implicit def optionsDsl2ConfigDsl[B, O](
    b: B)
   (implicit ev: CanProduce.Aux[B, O.OptionDsl[O]]):
    ConfigDsl[O] = {

    FreeApplicative.lift(Opts(ev(b)))
  }

  implicit def argumentsDsl2ConfigDsl[B, A](
    b: B)
   (implicit ev: CanProduce.Aux[B, A.ArgumentDsl[A]]): ConfigDsl[A] = {
    FreeApplicative.lift(Args(ev(b)))
  }

  implicit def canProduceConfigDslId[O] = CanProduce.Id[ConfigDsl[O]]
}
