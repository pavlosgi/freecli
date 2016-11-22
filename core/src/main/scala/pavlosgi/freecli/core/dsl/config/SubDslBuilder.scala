package pavlosgi.freecli.core.dsl.config

import scala.annotation.implicitNotFound

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.{LeftFolder, Prepend}

import pavlosgi.freecli.core.api.config.Algebra
import pavlosgi.freecli.core.dsl.generic

case class SubDslBuilder[H <: HList, T](list: H) {

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

object SubDslBuilder {
  def sub[T]: SubDslBuilder[HNil, T] = sub[HNil, T](HNil)
  def sub[H <: HList, T](list: H): SubDslBuilder[H, T] =
    new SubDslBuilder[H, T](list)

  @implicitNotFound("Could not produce Config from ${H}. Ensure that you provided a subconfiguration.")
  trait CanProduceConfigDsl[H <: HList, T] {
    def apply(a: SubDslBuilder[H, T]): ConfigDsl[T]
  }

  object CanProduceConfigDsl {
    implicit def canProduceConfigDsl[H <: HList, T, Out <: HList]
     (implicit canProduceDescription: CanProduceDescription.Aux[H, Out],
      dsl: Out =:= (ConfigDsl[T] :: HNil)):
      CanProduceConfigDsl[H, T] = {

      (s: SubDslBuilder[H, T]) => {
        val (description, remaining) = canProduceDescription.apply(s.list)
        val subDsl = dsl(remaining).head
        new ConfigDsl[T] {
          override def apply[F[_] : Algebra]: F[T] =
            implicitly[Algebra[F]].sub(description, subDsl)
        }
      }
    }

  }

  implicit def toConfigDsl[H <: HList, T](
    s: SubDslBuilder[H, T])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[H, T]):
    ConfigDsl[T] = {

    canProduceConfigDsl(s)
  }

  implicit def toConfigDslMerger[H <: HList, T](
    s: SubDslBuilder[H, T])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[H, T]):
    ConfigDsl.Merger[T] = {

    toConfigDsl(s)
  }

  implicit def builder2FA[H <: HNil, Out <: HList, T, F[_]: Algebra](
    implicit canProduceConfigDsl: CanProduceConfigDsl[H, T]):
    SubDslBuilder[H, T] => F[T] =

    toConfigDsl(_).apply[F]
}