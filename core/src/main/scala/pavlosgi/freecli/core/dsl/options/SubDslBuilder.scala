package pavlosgi.freecli.core.dsl.options

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.{LeftFolder, Prepend, Selector}

import pavlosgi.freecli.core.api.options._
import pavlosgi.freecli.core.dsl.generic

case class SubDslBuilder[H <: HList, T](list: H) extends Builder[H] {

  type Out[A <: HList] = SubDslBuilder[A, T]

  def apply[Conf](
    f: OptionsDsl[Conf])
  (implicit folder: LeftFolder.Aux[Conf :: HNil, Option[T], generic.type, T],
    //TODO Make this constraint any OptionsDsl[_]
    ev2: NotContainsConstraint[H, OptionsDsl[_]],
    ev3: Prepend[H, OptionsDsl[T] :: HNil]) = {

    append(f.map(c => folder(c :: HNil, Option.empty[T])))
  }

  override def append[A](t: A)(implicit ev: Prepend[H, ::[A, HNil]]): SubDslBuilder[ev.Out, T] = {
    new SubDslBuilder(list :+ t)
  }
}

object SubDslBuilder {
  def sub[T]: SubDslBuilder[HNil, T] = sub[HNil, T](HNil)
  def sub[H <: HList, T](list: H): SubDslBuilder[H, T] =
    new SubDslBuilder[H, T](list)

  implicit def toOptionsDsl[H <: HList, T, Out <: HList](
    s: SubDslBuilder[H, T])
   (implicit canProduceDescription: CanProduceDescription.Aux[H, Out],
    selector: Selector[Out, OptionsDsl[T]]):
    OptionsDsl[T] = {

    val (description, remaining) = canProduceDescription.apply(s.list)

    val subDsl: OptionsDsl[T] = selector(remaining)

    new OptionsDsl[T] {
      override def apply[F[_] : Algebra]: F[T] =
        implicitly[Algebra[F]].sub(description, subDsl)

    }
  }

  implicit def toOptionsDslMerger[H <: HList, T, Out <: HList](
    s: SubDslBuilder[H, T])
   (implicit canProduceDescription: CanProduceDescription.Aux[H, Out],
    selector: Selector[Out, OptionsDsl[T]]):
    OptionsDsl.Merger[T] = {

    toOptionsDsl(s)
  }

  implicit def builder2FA[H <: HNil, Out <: HList, T, F[_]: Algebra](
    implicit canProduceDescription: CanProduceDescription.Aux[H, Out],
    selector: Selector[Out, OptionsDsl[T]]):
    SubDslBuilder[H, T] => F[T] =

    toOptionsDsl(_).apply[F]
}