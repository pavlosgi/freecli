package pavlosgi.freecli.core

import shapeless._

sealed trait Partition[L <: HList, F[_]] extends DepFn1[L] {
  type NotF <: HList
  type IsF <: HList
  type Out = (NotF, IsF)
}

sealed trait LowPriorityPartition {
  type Aux[L <: HList, F[_], NotF0, IsF0] = Partition[L, F] {
    type NotF = NotF0
    type IsF = IsF0
  }

  implicit def hconsPartition0[F[_], H, T <: HList](
    implicit tp: Partition[T, F]):
    Aux[H :: T, F, H :: tp.NotF, tp.IsF] =

    new Partition[H :: T, F] {
      type NotF = H :: tp.NotF
      type IsF = tp.IsF
      def apply(l: H :: T): Out = {
        val (tNotF, tIsF) = tp(l.tail)
        (l.head :: tNotF, tIsF)
      }
    }
}

object Partition extends LowPriorityPartition {
  implicit def hnilPartition[F[_]]: Aux[HNil, F, HNil, HNil] =
    new Partition[HNil, F] {
      type NotF = HNil
      type IsF = HNil
      def apply(l: HNil): Out = (HNil, HNil)
    }

  implicit def hconsPartition1[F[_], H, T <: HList](
    implicit tp: Partition[T, F]):
    Aux[F[H] :: T, F, tp.NotF, F[H] :: tp.IsF] =

    new Partition[F[H] :: T, F] {
      type NotF = tp.NotF
      type IsF = F[H] :: tp.IsF
      def apply(l: F[H] :: T): Out = {
        val (tNotF, tIsF) = tp(l.tail)
        (tNotF, l.head :: tIsF)
      }
    }
}
