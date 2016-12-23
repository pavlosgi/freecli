package pavlosgi.freecli.command.dsl

import cats.free.FreeApplicative
import shapeless.ops.hlist.{LeftFolder, Tupler}
import shapeless.{::, HList, HNil}

import pavlosgi.freecli.command.api.{CommandFieldName, RunCommand}
import pavlosgi.freecli.config.{api => C}
import pavlosgi.freecli.config.dsl._
import pavlosgi.freecli.core._
import pavlosgi.freecli.core.ops.genericPoly

trait Ops {
  def cmd(name: String) =
    CommandDslBuilder[CommandFieldName :: HNil, Unit, Unit](
      CommandFieldName(name) :: HNil)

  def cmd(name: String, description: Description) =
    CommandDslBuilder[CommandFieldName :: Description :: HNil, Unit, Unit](
      CommandFieldName(name) :: description :: HNil)

  def takes[T](c: ConfigDsl[T]): CommandDslBuilder[ConfigDsl[T] :: HNil, T, Unit] = {
    CommandDslBuilder(c :: HNil)
  }

  def takesG[T] = new Takes[T]
  def takesT[Algebra[_], T <: HList, Tup](
    c: ConfigDsl[T])
   (implicit ev: Tupler.Aux[T, Tup]):
    CommandDslBuilder[ConfigDsl[Tup] :: HNil, Tup, Unit] = {

    CommandDslBuilder(c.map(ev.apply) :: HNil)
  }

  def runs[T](f: T => Unit) =
   CommandDslBuilder[RunCommand[T] :: HNil, Unit, T](
    RunCommand[T](f) :: HNil)

  def runs(f: => Unit) =
    CommandDslBuilder[RunCommand[HNil] :: HNil, Unit, HNil](
      RunCommand[HNil](_ => f) :: HNil)
}

class Takes[T] {
  def apply[Algebra[_], Conf](
    f: FreeApplicative[C.Algebra, Conf])
   (implicit folder: LeftFolder.Aux[Conf :: HNil, Option[T], genericPoly.type, T]):
    CommandDslBuilder[ConfigDsl[T] :: HNil, T, Unit] = {

    CommandDslBuilder(f.map(c => folder(c :: HNil, Option.empty[T])) :: HNil)
  }
}