package freecli
package config
package dsl

import cats.free.FreeApplicative

import api.{Args, Opts}
import argument.{dsl => A}
import core.api.CanProduce
import option.{dsl => O}

trait ConfigDslImplicits {
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

  implicit def canProduceConfigDslId[O]: CanProduce[ConfigDsl[O]] =
    CanProduce.Id[ConfigDsl[O]]
}
