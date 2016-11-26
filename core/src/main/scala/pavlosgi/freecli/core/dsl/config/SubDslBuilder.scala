package pavlosgi.freecli.core.dsl.config

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.{LeftFolder, Prepend}

import pavlosgi.freecli.core.api.config.Sub
import pavlosgi.freecli.core.dsl.generic

private[config] case class SubDslBuilder[H <: HList, T](list: H) {

  def apply[Conf](
    f: ConfigDsl[Conf])
  (implicit folder: LeftFolder.Aux[Conf :: HNil, Option[T], generic.type, T],
    //TODO Make this constraint any ConfigDsl[_]
    ev2: NotContainsConstraint[H, ConfigDsl[_]],
    ev3: Prepend[H, ConfigDsl[T] :: HNil]) = {

    new SubDslBuilder[ev3.Out, T](
      list :+ f.map(c => folder(c :: HNil, Option.empty[T])))
  }
}

private[config] object SubDslBuilder {
  def sub[T]: SubDslBuilder[HNil, T] = sub[HNil, T](HNil)
  def sub[H <: HList, T](list: H): SubDslBuilder[H, T] =
    new SubDslBuilder[H, T](list)

  implicit def canProduceConfigDsl[H <: HList, T, Out <: HList]
   (implicit canProduceDescription: CanProduceDescription.Aux[H, Out],
    dsl: Out =:= (ConfigDsl[T] :: HNil)):
    CanProduceConfigDsl[SubDslBuilder[H, ?], T, T] = {

    (s: SubDslBuilder[H, T]) => {
      val (description, remaining) = canProduceDescription.apply(s.list)
      val subDsl = dsl(remaining).head
      FreeApplicative.lift(Sub[T](description, subDsl))
    }
  }

  implicit def toConfigDsl[H <: HList, T](
    s: SubDslBuilder[H, T])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[SubDslBuilder[H, ?], T, T]):
    ConfigDsl[T] = {

    canProduceConfigDsl(s)
  }

  implicit def toConfigDslMerger[H <: HList, T](
    s: SubDslBuilder[H, T])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[SubDslBuilder[H, ?], T, T]):
    Merger[T] = {

    toConfigDsl(s)
  }
}