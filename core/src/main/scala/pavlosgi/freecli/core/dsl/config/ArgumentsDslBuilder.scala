package pavlosgi.freecli.core.dsl.config

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._

private[config] case class ArgumentsDslBuilder[H <: HList, T](list: H) {

  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    new ArgumentsDslBuilder[ev.Out, T](list :+ description)

  def -~(
    name: ArgumentName)
   (implicit ev: Prepend[H, ArgumentName :: HNil],
    ev2: NotContainsConstraint[H, ArgumentName]) =

    new ArgumentsDslBuilder[ev.Out, T](list :+ name)
}

private[config] object ArgumentsDslBuilder {
  def arg[T](implicit decoder: StringDecoder[T]): ArgumentsDslBuilder[HNil, T] =
    arg(HNil)

  def arg[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new ArgumentsDslBuilder[H, T](list)

  private[config] sealed trait CanProduceArgDsl[H <: HList, T, A] {
    def apply(a: ArgumentsDslBuilder[H, T]): ConfigDsl[A]
  }

  object CanProduceArgDsl {
    implicit def canProduceArgDsl[H <: HList, T, Out <: HList](
      implicit canProduceArgumentDetails: CanProduceArgumentDetails.Aux[T, H, HNil],
      decoder: StringDecoder[T]):
      CanProduceArgDsl[H, T, T] = {

      new CanProduceArgDsl[H, T, T] {
        def apply(o: ArgumentsDslBuilder[H, T]) = {
          val (ad, _) = canProduceArgumentDetails.apply(o.list)

          FreeApplicative.lift(Arg[T, T](ad, identity, decoder))
        }
      }
    }
  }

  implicit def toArgDsl[H <: HList, T, A](
    o: ArgumentsDslBuilder[H, T])
   (implicit canProduceArgDsl: CanProduceArgDsl[H, T, A]):
    ConfigDsl[A] = {

    canProduceArgDsl.apply(o)
  }

  implicit def toArgDslMerger[H <: HList, T, A](
    o: ArgumentsDslBuilder[H, T])
   (implicit canProduceArgDsl: CanProduceArgDsl[H, T, A]):
    Merger[A] = {

    canProduceArgDsl.apply(o)
  }
}