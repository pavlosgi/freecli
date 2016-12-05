package pavlosgi.freecli.core

import cats.free.FreeApplicative
import shapeless.ops.hlist.LeftFolder
import shapeless.{::, HNil}

class Gen[T] {
  def apply[Algebra[_], Conf](
    f: FreeApplicative[Algebra, Conf])
   (implicit folder: LeftFolder.Aux[Conf :: HNil, Option[T], generic.type, T]):
    FreeApplicative[Algebra, T] = {

    f.map(c => folder(c :: HNil, Option.empty[T]))
  }
}
