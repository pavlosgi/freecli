package pavlosgi.freecli.core.dsl.config

import cats.free.FreeApplicative
import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.ConfigDslBuilder.CanMerge
import pavlosgi.freecli.core.dsl.{arguments => A, options => O}
import pavlosgi.freecli.core.dsl.{CanProduce, toHList}

case class ConfigDslBuilder[H <: HList, O, A](list: H) {
  def ::[H2 <: HList, O2, A2](
    b: ConfigDslBuilder[H2, O2, A2])
   (implicit ev: CanMerge[H2, O2, A2, H, O, A]) = {
    ev(b, this)
  }
}

object ConfigDslBuilder {
  sealed trait CanMerge[H1 <: HList, O1, A1, H2 <: HList, O2, A2] {
    type AOut
    type OOut
    type HOut <: HList
    type Out = ConfigDslBuilder[HOut, OOut, AOut]

    def apply(
      c1: ConfigDslBuilder[H1, O1, A1],
      c2: ConfigDslBuilder[H2, O2, A2]):
      Out
  }

  object CanMerge {
    type Aux[H1 <: HList, O1, A1, H2 <: HList, O2, A2, OOut0, AOut0, HOut0] =
      CanMerge[H1, O1, A1, H2, O2, A2] {
        type OOut = OOut0
        type AOut = AOut0
        type HOut = HOut0
      }

    implicit def canMergeOptionsDslToOptionsDsl[O1, O2, Out0 <: HList](
      implicit ev: LeftFolder.Aux[O1 :: O2 :: HNil, HNil, toHList.type, Out0]) =
      new CanMerge[O.OptionsDsl[O1] :: HNil, O1, Unit, O.OptionsDsl[O2] :: HNil, O2, Unit] {
        type AOut = Unit
        type OOut = Out0
        type HOut = O.OptionsDsl[Out0] :: HNil

        def apply(
          t: ConfigDslBuilder[O.OptionsDsl[O1] :: HNil, O1, Unit],
          u: ConfigDslBuilder[O.OptionsDsl[O2] :: HNil, O2, Unit]):
          Out = {

          val dsl = (t.list.head |@| u.list.head).map((t0, u0) =>
            ev(t0 :: u0 :: HNil, HNil))

          ConfigDslBuilder(dsl :: HNil)
        }
      }

    implicit def canMergeArgumentsDslToArgumentsDsl[A1, A2, Out0 <: HList](
      implicit ev: LeftFolder.Aux[A1 :: A2 :: HNil, HNil, toHList.type, Out0]) =
      new CanMerge[A.ArgumentsDsl[A1] :: HNil, Unit, A1, A.ArgumentsDsl[A2] :: HNil, Unit, A2] {
        type AOut = Out0
        type OOut = Unit
        type HOut = A.ArgumentsDsl[Out0] :: HNil

        def apply(
          t: ConfigDslBuilder[A.ArgumentsDsl[A1] :: HNil, Unit, A1],
          u: ConfigDslBuilder[A.ArgumentsDsl[A2] :: HNil, Unit, A2]):
          Out = {

          val dsl = (t.list.head |@| u.list.head).map((t0, u0) =>
            ev(t0 :: u0 :: HNil, HNil))

          ConfigDslBuilder(dsl :: HNil)
        }
      }

    implicit def canMergeOptionsToArguments[O1, A2, Out0 <: HList](
      implicit ev: LeftFolder.Aux[O1 :: A2 :: HNil, HNil, toHList.type, Out0]) =
      new CanMerge[O.OptionsDsl[O1] :: HNil, O1, Unit, A.ArgumentsDsl[A2] :: HNil, Unit, A2] {
        type AOut = A2
        type OOut = O1
        type HOut = O.OptionsDsl[O1] :: A.ArgumentsDsl[A2] :: HNil

        def apply(
          t: ConfigDslBuilder[O.OptionsDsl[O1] :: HNil, O1, Unit],
          u: ConfigDslBuilder[A.ArgumentsDsl[A2] :: HNil, Unit, A2]):
          Out = {

          ConfigDslBuilder(t.list.head :: u.list.head :: HNil)
        }
      }

    implicit def canMergeOptionsToOptionsAndArguments[O1, O2, A2, OOut0 <: HList](
      implicit ev: LeftFolder.Aux[O1 :: O2 :: HNil, HNil, toHList.type, OOut0]) =
      new CanMerge[O.OptionsDsl[O1] :: HNil, O1, Unit, O.OptionsDsl[O2] :: A.ArgumentsDsl[A2] :: HNil, O2, A2] {
        type AOut = A2
        type OOut = OOut0
        type HOut = O.OptionsDsl[OOut] :: A.ArgumentsDsl[A2] :: HNil

        def apply(
          t: ConfigDslBuilder[O.OptionsDsl[O1] :: HNil, O1, Unit],
          u: ConfigDslBuilder[O.OptionsDsl[O2] :: A.ArgumentsDsl[A2] :: HNil, O2, A2]):
          Out = {

          val opts = (t.list.head |@| u.list.head).map((o1, o2) => ev(o1 :: o2 :: HNil, HNil))

          ConfigDslBuilder(opts :: u.list.tail.head :: HNil)
        }
      }
  }

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