package pavlosgi.freecli.core

import cats.free.FreeApplicative
import shapeless.HList
import shapeless.ops.hlist.Tupler

trait Grouping {
  def group[T] = new Group[T]
  def groupT[Algebra[_], T <: HList, Tup](
    c: FreeApplicative[Algebra, T])
   (implicit ev: Tupler.Aux[T, Tup]):
    FreeApplicative[Algebra, Tup] = {

    c.map(ev.apply)
  }
}
