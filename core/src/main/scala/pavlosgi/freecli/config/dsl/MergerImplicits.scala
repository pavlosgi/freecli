package pavlosgi.freecli.config.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.argument.{dsl => A}
import pavlosgi.freecli.core.Merger.CanMerge
import pavlosgi.freecli.core.{CanProduce, Merger, toHList}
import pavlosgi.freecli.option.{dsl => O}

trait MergerImplicits {
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

  implicit def canMergeOptionsToArguments[O1, A2, Out0 <: HList](
    implicit ev: LeftFolder.Aux[O1 :: A2 :: HNil, HNil, toHList.type, Out0]) = {

    new CanMerge[O.OptionDsl[O1], A.ArgumentDsl[A2]] {
      type Out = ConfigDslBuilder[O.OptionDsl[O1] :: A.ArgumentDsl[A2] :: HNil, O1, A2]

      def apply(
        b1: O.OptionDsl[O1],
        b2: A.ArgumentDsl[A2]):
        Out = {

        ConfigDslBuilder(b1 :: b2 :: HNil)
      }
    }
  }

    implicit def canMergeOptionsToOptionsAndArguments[O1, O2, A2, OOut0 <: HList](
      implicit ev: LeftFolder.Aux[O1 :: O2 :: HNil, HNil, toHList.type, OOut0]) = {

      new CanMerge[
        O.OptionDsl[O1],
        ConfigDslBuilder[O.OptionDsl[O2] :: A.ArgumentDsl[A2] :: HNil, O2, A2]] {
        type Out = ConfigDslBuilder[O.OptionDsl[OOut0] :: A.ArgumentDsl[A2] :: HNil, OOut0, A2]

        def apply(
          b1: O.OptionDsl[O1],
          b2: ConfigDslBuilder[O.OptionDsl[O2] :: A.ArgumentDsl[A2] :: HNil, O2, A2]):
          Out = {

          val opts = (b1 |@| b2.list.head)
            .map((o1, o2) => ev(o1 :: o2 :: HNil, HNil))

          ConfigDslBuilder(opts :: b2.list.tail.head :: HNil)
        }
      }
    }
}
