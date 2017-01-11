package pavlosgi.freecli.config.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.argument.{dsl => A}
import pavlosgi.freecli.core.api.CanMerge
import pavlosgi.freecli.core.poly.toHList
import pavlosgi.freecli.option.{dsl => O}

trait MergerImplicits {

  implicit def canMergeOptionsToArguments[O1, A2, Out0 <: HList](
    implicit ev: LeftFolder.Aux[O1 :: A2 :: HNil, HNil, toHList.type, Out0]):
    CanMerge.Aux[
      O.OptionDsl[O1],
      A.ArgumentDsl[A2],
      ConfigDslBuilder[O.OptionDsl[O1] :: A.ArgumentDsl[A2] :: HNil, O1, A2]] = {

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
    implicit ev: LeftFolder.Aux[O1 :: O2 :: HNil, HNil, toHList.type, OOut0]):
    CanMerge.Aux[
      O.OptionDsl[O1],
      ConfigDslBuilder[O.OptionDsl[O2] :: A.ArgumentDsl[A2] :: HNil, O2, A2],
      ConfigDslBuilder[O.OptionDsl[OOut0] :: A.ArgumentDsl[A2] :: HNil, OOut0, A2]] = {

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
