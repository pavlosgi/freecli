package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.DefaultBuilder.CanProduceDefault
import pavlosgi.freecli.core.dsl.config.FieldBuilder.CanProduceField

case class ArgDsl[H <: HList, T](list: H)
  extends FieldNameOps[H]
   with FieldDescriptionOps[H]
   with DefaultOps[H, T]
   with Builder[H] {

   type Out[A <: HList] = ArgDsl[A, T]

   override def append[A](t: A)(implicit ev: Prepend[H, ::[A, HNil]]): ArgDsl[ev.Out, T] = {
     new ArgDsl(list :+ t)
   }
  }

object ArgDsl {
  def arg[T](implicit decoder: StringDecoder[T]): ArgDsl[HNil, T] = arg(HNil)

  def arg[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new ArgDsl[H, T](list)

  implicit def toConfigDsl[T, H <: HList, Out <: HList](
    a: ArgDsl[H, T])
   (implicit canProduceField: CanProduceField.Aux[H, Out],
    canProduceDefault: CanProduceDefault[T, Out],
    decoder: StringDecoder[T]):
    ConfigDsl[T] = {

    val (field, remaining) = canProduceField.apply(a.list)
    val (default, _) = canProduceDefault.apply(remaining)

    new ConfigDsl[T] {
      override def apply[F[_] : Algebra]: F[T] =
        implicitly[Algebra[F]].arg[T, T](field, identity, decoder, default)

    }
  }

  implicit def toConfigDslMerger[T, H <: HList, Out <: HList](
    a: ArgDsl[H, T])
   (implicit canProduceField: CanProduceField.Aux[H, Out],
    canProduceDefault: CanProduceDefault[T, Out],
    decoder: StringDecoder[T]):
    ConfigDsl.Merger[T] = {

    toConfigDsl(a)
  }

  implicit def dsl2FA[T, H <: HList, Out <: HList, F[_]: Algebra](
    implicit canProduceField: CanProduceField.Aux[H, Out],
    canProduceDefault: CanProduceDefault[T, Out],
    decoder: StringDecoder[T]):
    ArgDsl[H, T] => F[T] = {

    toConfigDsl(_).apply[F]
  }
}