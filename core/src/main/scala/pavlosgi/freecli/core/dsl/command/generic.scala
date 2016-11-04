package pavlosgi.freecli.core.dsl.command

import shapeless._

object generic extends Poly2 {
  implicit def fromHList[L <: HList, A](implicit ev: Generic.Aux[A, L]) =
    at[Option[A], L]((_, h) => ev.from(h))

  implicit def fromNonHList[L, A](
    implicit ev: L =:!= A,
    ev2: Generic.Aux[A, L :: HNil]):
    Case.Aux[Option[A], L, A] = {

    at[Option[A], L]((_, h) => ev2.from(h :: HNil))
  }

  implicit def other[L, A](implicit ev: L =:= A): Case.Aux[Option[A], L, A] =
    at[Option[A], L]((_, h) => ev(h))
}
