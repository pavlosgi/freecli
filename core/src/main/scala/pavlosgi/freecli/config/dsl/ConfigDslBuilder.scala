package pavlosgi.freecli.config.dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.argument.{dsl => A}
import pavlosgi.freecli.config.api._
import pavlosgi.freecli.core.api.{CanProduce, Merger}
import pavlosgi.freecli.core.poly.toHList
import pavlosgi.freecli.option.{dsl => O}

case class ConfigDslBuilder[H <: HList, O, A](list: H)
object ConfigDslBuilder {
  def arguments[A](a: A.ArgumentDsl[A]) = {
    ConfigDslBuilder[A.ArgumentDsl[A] :: HNil, Unit, A](a :: HNil)
  }

  def options[O](o: O.OptionDsl[O]) = {
    ConfigDslBuilder[O.OptionDsl[O] :: HNil, O, Unit](o :: HNil)
  }

  implicit def canProduceConfigDslFromOptions[O]:
    CanProduce.Aux[ConfigDslBuilder[O.OptionDsl[O] :: HNil, O, Unit], ConfigDsl[O]] = {

    new CanProduce[ConfigDslBuilder[O.OptionDsl[O] :: HNil, O, Unit]] {
      type Out = ConfigDsl[O]
      def apply(v: ConfigDslBuilder[O.OptionDsl[O] :: HNil, O, Unit]): Out = {
        FreeApplicative.lift(Opts[O](v.list.head))
      }
    }
  }

  implicit def canProduceConfigDslFromArguments[A]:
    CanProduce.Aux[ConfigDslBuilder[A.ArgumentDsl[A] :: HNil, Unit, A], ConfigDsl[A]] = {

    new CanProduce[ConfigDslBuilder[A.ArgumentDsl[A] :: HNil, Unit, A]] {
      type Out = ConfigDsl[A]
      def apply(
        v: ConfigDslBuilder[A.ArgumentDsl[A] :: HNil, Unit, A]):
        Out = {

        FreeApplicative.lift(Args[A](v.list.head))
      }
    }
  }

  implicit def canProduceConfigDslFromOptionsAndArguments[O, A, Out0 <: HList](
    implicit ev: LeftFolder.Aux[O :: A :: HNil, HNil, toHList.type, Out0]):
    CanProduce.Aux[
      ConfigDslBuilder[O.OptionDsl[O] :: A.ArgumentDsl[A] :: HNil, O, A],
      ConfigDsl[Out0]] = {

    new CanProduce[ConfigDslBuilder[O.OptionDsl[O] :: A.ArgumentDsl[A] :: HNil, O, A]] {
      type Out = ConfigDsl[Out0]

      def apply(
        v: ConfigDslBuilder[O.OptionDsl[O] :: A.ArgumentDsl[A] :: HNil, O, A]):
        Out = {

        def f(o: O, a: A) = {
          ev(o :: a :: HNil, HNil)
        }

        FreeApplicative.lift(
          OptsAndArgs[O, A, Out0](v.list.head, v.list.tail.head, f))
      }
    }
  }

  implicit def configDslBuilder2Merger[H <: HList, O, A](
    b: ConfigDslBuilder[H, O, A]):
    Merger[ConfigDslBuilder[H, O, A]] = {

    Merger(b)
  }

  implicit def argumentsDsl2ConfigDslBuilder[B, A](
    a: B)(
    implicit ev: CanProduce.Aux[B, A.ArgumentDsl[A]]):
    ConfigDslBuilder[A.ArgumentDsl[A] :: HNil, Unit, A] = {

    new ConfigDslBuilder(ev(a) :: HNil)
  }

  implicit def optionsDsl2ConfigDslBuilder[B, O](
    o: B)(
    implicit ev: CanProduce.Aux[B, O.OptionDsl[O]]):
    ConfigDslBuilder[O.OptionDsl[O] :: HNil, O, Unit] = {

    new ConfigDslBuilder(ev(o) :: HNil)
  }
}