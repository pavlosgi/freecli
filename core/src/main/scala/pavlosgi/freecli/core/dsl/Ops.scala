package pavlosgi.freecli.core.dsl

import cats.free.FreeApplicative
import shapeless.HList
import shapeless.ops.hlist.Tupler

import pavlosgi.freecli.core.api.Description

trait Ops {
  def group[T] = new Group[T]
  def groupT[Algebra[_], T <: HList, Tup](
    c: FreeApplicative[Algebra, T])
   (implicit ev: Tupler.Aux[T, Tup]):
    FreeApplicative[Algebra, Tup] = {

    c.map(ev.apply)
  }

  def des(description: String): Description = Description(description)
}
