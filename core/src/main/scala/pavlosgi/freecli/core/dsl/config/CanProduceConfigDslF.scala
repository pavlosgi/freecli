package pavlosgi.freecli.core.dsl.config

import shapeless.HList

trait CanProduceConfigDslF2[F[_ <: HList, _], H <: HList, T, A] {
  def apply(a: F[H, T]): ConfigDsl[A]
}

trait CanProduceConfigDslF[F[_ <: HList], H <: HList, A] {
  def apply(a: F[H]): ConfigDsl[A]
}
