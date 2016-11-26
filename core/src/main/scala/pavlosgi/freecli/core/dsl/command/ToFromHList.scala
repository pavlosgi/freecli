package pavlosgi.freecli.core.dsl.command

import shapeless._

private[command] sealed trait ToFromHList[T, H <: HList] {
  def from(h: H): T
  def to(t: T): H
}

private[command] object ToFromHList {
  implicit def toFromHList[T <: HList, H <: HList](
    implicit ev: T =:= H,
    ev2: H =:= T) = {
    new ToFromHList[T, H] {
      override def from(h: H): T = ev(h)
      override def to(t: T): H = ev2(t)
    }
  }

  implicit def toFromHList2Product[T <: Product, H <: HList](
    implicit ev: Generic.Aux[T, H]) = {
    new ToFromHList[T, H] {
      override def from(h: H): T = ev.from(h)
      override def to(t: T): H = ev.to(t)
    }
  }

  implicit def toFromHListOther[T, H <: HList](
    implicit ev: H =:= (T :: HNil),
    ev2: (T :: HNil) =:= H) = {
    new ToFromHList[T, H] {
      override def from(h: H): T = ev(h).head
      override def to(t: T): H = ev2(t :: HNil)
    }
  }
}
