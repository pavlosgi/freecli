package pavlosgi.freecli.core.dsl.config

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.{LeftFolder, Prepend, Selector}

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.generic

case class SubDsl[H <: HList, T](list: H) extends Builder[H] {

  type Out[A <: HList] = SubDsl[A, T]

  def apply[Conf](
    f: ConfigDsl[Conf])
  (implicit folder: LeftFolder.Aux[Conf :: HNil, Option[T], generic.type, T],
    //TODO Make this constraint any ConfigDsl[_]
    ev2: NotContainsConstraint[H, ConfigDsl[_]],
    ev3: Prepend[H, ConfigDsl[T] :: HNil]) = {

    append(f.map(c => folder(c :: HNil, Option.empty[T])))
  }

  override def append[A](t: A)(implicit ev: Prepend[H, ::[A, HNil]]): SubDsl[ev.Out, T] = {
    new SubDsl(list :+ t)
  }
}

object SubDsl {
  def sub[T]: SubDsl[HNil, T] = sub[HNil, T](HNil)
  def sub[H <: HList, T](list: H): SubDsl[H, T] =
    new SubDsl[H, T](list)

  implicit def toConfigDsl[H <: HList, T, Out <: HList](
    s: SubDsl[H, T])
   (implicit canProduceDescription: CanProduceDescription.Aux[H, Out],
    selector: Selector[Out, ConfigDsl[T]]):
    ConfigDsl[T] = {

    val (description, remaining) = canProduceDescription.apply(s.list)

    val subDsl: ConfigDsl[T] = selector(remaining)

    new ConfigDsl[T] {
      override def apply[F[_] : Algebra]: F[T] =
        implicitly[Algebra[F]].sub(description, subDsl)

    }
  }

  implicit def toConfigDslMerger[H <: HList, T, Out <: HList](
    s: SubDsl[H, T])
   (implicit canProduceDescription: CanProduceDescription.Aux[H, Out],
    selector: Selector[Out, ConfigDsl[T]]):
    ConfigDsl.Merger[T] = {

    toConfigDsl(s)
  }

  implicit def dsl2FA[H <: HNil, Out <: HList, T, F[_]: Algebra](
    implicit canProduceDescription: CanProduceDescription.Aux[H, Out],
    selector: Selector[Out, ConfigDsl[T]]):
    SubDsl[H, T] => F[T] =

    toConfigDsl(_).apply[F]
}