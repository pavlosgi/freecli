package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.{Prepend, Selector}

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.DescriptionBuilder.CanProduceDescription

case class SubDsl[H <: HList, T](list: H)
 extends FieldNameOps[H]
 with FieldDescriptionOps[H]
 with Builder[H]
 with ApplyOps[H, T] {

 type Out[A <: HList] = SubDsl[A, T]

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