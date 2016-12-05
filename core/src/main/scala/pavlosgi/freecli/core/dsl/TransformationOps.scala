package pavlosgi.freecli.core.dsl

import cats.free.FreeApplicative
import shapeless.HList
import shapeless.ops.hlist.Tupler

trait TransformationOps {
  def gen[T] = new Gen[T]
  def tupled[Algebra[_], T <: HList, Tup](
    c: FreeApplicative[Algebra, T])
   (implicit ev: Tupler.Aux[T, Tup]):
    FreeApplicative[Algebra, Tup] = {

    c.map(ev.apply)
  }
}
