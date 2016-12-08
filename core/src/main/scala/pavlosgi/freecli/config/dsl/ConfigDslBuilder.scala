package pavlosgi.freecli.config.dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.config.api._
import pavlosgi.freecli.arguments.{dsl => A}
import pavlosgi.freecli.core.{CanProduce, toHList}
import pavlosgi.freecli.options.{dsl => O}

case class ConfigDslBuilder[H <: HList, O, A](list: H)
object ConfigDslBuilder {
  def arguments[A](a: A.ArgumentsDsl[A]) = {
    ConfigDslBuilder[A.ArgumentsDsl[A] :: HNil, Unit, A](a :: HNil)
  }

  def options[O](o: O.OptionsDsl[O]) = {
    ConfigDslBuilder[O.OptionsDsl[O] :: HNil, O, Unit](o :: HNil)
  }

  implicit def canProduceConfigDslFromOptions[O]:
    CanProduce.Aux[ConfigDslBuilder[O.OptionsDsl[O] :: HNil, O, Unit], ConfigDsl[O]] = {

    new CanProduce[ConfigDslBuilder[O.OptionsDsl[O] :: HNil, O, Unit]] {
      type Out = ConfigDsl[O]
      def apply(v: ConfigDslBuilder[O.OptionsDsl[O] :: HNil, O, Unit]): Out = {
        FreeApplicative.lift(Opts[O](v.list.head))
      }
    }
  }

  implicit def canProduceConfigDslFromArguments[A]:
    CanProduce.Aux[ConfigDslBuilder[A.ArgumentsDsl[A] :: HNil, Unit, A], ConfigDsl[A]] = {

    new CanProduce[ConfigDslBuilder[A.ArgumentsDsl[A] :: HNil, Unit, A]] {
      type Out = ConfigDsl[A]
      def apply(
        v: ConfigDslBuilder[A.ArgumentsDsl[A] :: HNil, Unit, A]):
        Out = {

        FreeApplicative.lift(Args[A](v.list.head))
      }
    }
  }

  implicit def canProduceConfigDslFromOptionsAndArguments[O, A, Out0 <: HList](
    implicit ev: LeftFolder.Aux[O :: A :: HNil, HNil, toHList.type, Out0]):
    CanProduce.Aux[ConfigDslBuilder[O.OptionsDsl[O] :: A.ArgumentsDsl[A] :: HNil, O, A], ConfigDsl[Out0]] = {

    new CanProduce[ConfigDslBuilder[O.OptionsDsl[O] :: A.ArgumentsDsl[A] :: HNil, O, A]] {
      type Out = ConfigDsl[Out0]

      def apply(
        v: ConfigDslBuilder[O.OptionsDsl[O] :: A.ArgumentsDsl[A] :: HNil, O, A]):
        Out = {

        def f(o: O, a: A) = {
          ev(o :: a :: HNil, HNil)
        }

        FreeApplicative.lift(
          OptsAndArgs[O, A, Out0](v.list.head, v.list.tail.head, f))
      }
    }
  }
}