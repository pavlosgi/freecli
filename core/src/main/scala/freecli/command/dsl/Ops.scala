package freecli
package command
package dsl

import cats.free.FreeApplicative
import shapeless.ops.hlist.{LeftFolder, Tupler}
import shapeless.{::, HList, HNil}

import api.{CommandFieldName, RunCommand}
import config.{api => C}
import config.dsl._
import core.api.Description
import core.poly.genericPoly

trait Ops {
  def cmd(name: String) =
    CommandDslBuilder[CommandFieldName :: HNil, HNil, HNil](
      CommandFieldName(name) :: HNil)

  def cmd(name: String, description: Description) =
    CommandDslBuilder[CommandFieldName :: Description :: HNil, HNil, HNil](
      CommandFieldName(name) :: description :: HNil)

  def takes[T](c: ConfigDsl[T]): CommandDslBuilder[ConfigDsl[T] :: HNil, T, HNil] = {
    CommandDslBuilder(c :: HNil)
  }

  def takesG[T] = new Takes[T]
  def takesT[Algebra[_], T <: HList, Tup](
    c: ConfigDsl[T])
   (implicit ev: Tupler.Aux[T, Tup]):
    CommandDslBuilder[ConfigDsl[Tup] :: HNil, Tup, HNil] = {

    CommandDslBuilder(c.map(ev.apply) :: HNil)
  }

  def runs[T](f: T => Unit) =
   CommandDslBuilder[RunCommand[T] :: HNil, HNil, T](
    RunCommand[T](f) :: HNil)

  def runs(f: => Unit) =
    CommandDslBuilder[RunCommand[HNil] :: HNil, HNil, HNil](
      RunCommand[HNil](_ => f) :: HNil)
}

class Takes[T] {
  def apply[Algebra[_], Conf](
    f: FreeApplicative[C.Algebra, Conf])
   (implicit folder: LeftFolder.Aux[Conf :: HNil, Option[T], genericPoly.type, T]):
    CommandDslBuilder[ConfigDsl[T] :: HNil, T, HNil] = {

    CommandDslBuilder(f.map(c => folder(c :: HNil, Option.empty[T])) :: HNil)
  }
}