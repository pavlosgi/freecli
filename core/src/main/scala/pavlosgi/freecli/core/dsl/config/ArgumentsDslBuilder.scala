package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._

case class ArgumentsDslBuilder[H <: HList, T](list: H) extends Builder[H] {

  type Out[A <: HList] = ArgumentsDslBuilder[A, T]

  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    append(description)

  def -~(
    name: ArgumentName)
   (implicit ev: Prepend[H, ArgumentName :: HNil],
    ev2: NotContainsConstraint[H, ArgumentName]) =

    append(name)

  override def append[A](
   t: A)
  (implicit ev: Prepend[H, ::[A, HNil]]):
   ArgumentsDslBuilder[ev.Out, T] = {

   new ArgumentsDslBuilder(list :+ t)
  }
}

object ArgumentsDslBuilder {
  def arg[T](implicit decoder: StringDecoder[T]): ArgumentsDslBuilder[HNil, T] =
    arg(HNil)

  def arg[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new ArgumentsDslBuilder[H, T](list)

  trait CanProduceArgDsl[F[_ <: HList, _], H <: HList, T, A] {
    def apply(a: F[H, T]): ConfigDsl[A]
  }

  object CanProduceArgDsl {
    implicit def canProduceArgDsl[H <: HList, T, Out <: HList](
      implicit canProduceArgumentDetails: CanProduceArgumentDetails.Aux[T, H, HNil],
      decoder: StringDecoder[T]):
      CanProduceArgDsl[ArgumentsDslBuilder, H, T, T] = {

      (o: ArgumentsDslBuilder[H, T]) => {
        val (ad, _) = canProduceArgumentDetails.apply(o.list)

        new ConfigDsl[T] {
          override def apply[F[_] : Algebra]: F[T] =
            implicitly[Algebra[F]].arg[T, T](ad, identity, decoder)

        }
      }
    }
  }

  implicit def toArgDsl[H <: HList, T, A](
    o: ArgumentsDslBuilder[H, T])
   (implicit canProduceArgDsl: CanProduceArgDsl[ArgumentsDslBuilder, H, T, A]):
    ConfigDsl[A] = {

    canProduceArgDsl.apply(o)
  }

  implicit def toArgDslMerger[H <: HList, T, A](
    o: ArgumentsDslBuilder[H, T])
   (implicit canProduceArgDsl: CanProduceArgDsl[ArgumentsDslBuilder, H, T, A]):
    ConfigDsl.Merger[A] = {

    canProduceArgDsl.apply(o)
  }

  implicit def dsl2FA[H <: HList, T, A, Out <: HList, F[_]: Algebra](
    implicit canProduceArgDsl: CanProduceArgDsl[ArgumentsDslBuilder, H, T, A]):
    ArgumentsDslBuilder[H, T] => F[A] = {

    canProduceArgDsl(_).apply[F]
  }
}