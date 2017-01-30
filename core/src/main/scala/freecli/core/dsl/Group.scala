package freecli
package core
package dsl

import cats.free.FreeApplicative
import shapeless.ops.hlist.LeftFolder
import shapeless.{::, HNil}

import core.poly.genericPoly

class Group[T] {
  def apply[Algebra[_], Conf](
    f: FreeApplicative[Algebra, Conf])
   (implicit folder: LeftFolder.Aux[Conf :: HNil, Option[T], genericPoly.type, T]):
    FreeApplicative[Algebra, T] = {

    f.map(c => folder(c :: HNil, Option.empty[T]))
  }
}
