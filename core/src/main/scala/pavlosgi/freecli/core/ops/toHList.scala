package pavlosgi.freecli.core.ops

import shapeless._
import shapeless.ops.hlist.Prepend

object toHList extends Poly2 {
  implicit def fromHList[L <: HList, H <: HList]
   (implicit ev: Prepend[L, H]) = {

    at[L, H]((l, h) => l ++ h)
  }

  implicit def fromUnit[L <: HList, H]
   (implicit ev2: H =:= Unit) = {

    at[L, H]((l, _) => l)
  }

  implicit def fromHNil[L <: HList, H]
   (implicit ev2: H =:= HNil) = {

    at[L, H]((l, _) => l)
  }

  implicit def fromNonHList[L <: HList, H]
   (implicit ev: Prepend[L, H :: HNil],
    ev2: H <:!< HList,
    ev3: H =:!= HNil,
    ev4: H =:!= Unit) = {

    at[L, H]((l, h) => l :+ h)
  }
}
