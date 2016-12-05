package pavlosgi.freecli.core.dsl

import shapeless.{::, <:!<, HList, HNil, Poly2}
import shapeless.ops.hlist.Prepend

object toHList extends Poly2 {
    implicit def fromHList[L <: HList, H <: HList]
     (implicit ev: Prepend[L, H]) = {

      at[L, H]((l, h) => l ++ h)
    }

    implicit def fromNonHList[L <: HList, H]
     (implicit ev: Prepend[L, H :: HNil],
      ev2: H <:!< HList) = {

      at[L, H]((l, h) => l :+ h)
    }
  }
