package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.FieldBuilder.CanProduceField

case class OptDsl[H <: HList, T](list: H)
 extends FieldNameOps[H]
 with FieldDescriptionOps[H]
 with Builder[H] {

 type Out[A <: HList] = OptDsl[A, T]

 override def append[A](t: A)(implicit ev: Prepend[H, ::[A, HNil]]): OptDsl[ev.Out, T] = {
   new OptDsl(list :+ t)
 }
}

object OptDsl {
  def opt[T](implicit decoder: StringDecoder[T]): OptDsl[HNil, T] =
    opt(HNil)

  def opt[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new OptDsl[H, T](list)

  implicit def toConfigDsl[H <: HList, T, Out <: HList](
    o: OptDsl[H, T])
   (implicit canProduceField: CanProduceField.Aux[H, Out],
    decoder: StringDecoder[T]):
    ConfigDsl[Option[T]] = {

    val (field, _) = canProduceField.apply(o.list)

    new ConfigDsl[Option[T]] {
      override def apply[F[_] : Algebra]: F[Option[T]] =
        implicitly[Algebra[F]].opt[T, Option[T]](field, identity, decoder)

    }
  }

  implicit def toConfigDslMerger[T, H <: HList, Out <: HList](
    o: OptDsl[H, T])
   (implicit canProduceField: CanProduceField.Aux[H, Out],
    decoder: StringDecoder[T]):
    ConfigDsl.Merger[Option[T]] = {

    toConfigDsl(o)
  }

  implicit def dsl2FA[T, H <: HList, Out <: HList, F[_]: Algebra](implicit canProduceField: CanProduceField.Aux[H, Out],
    decoder: StringDecoder[T]):
    OptDsl[H, T] => F[Option[T]] = {

    toConfigDsl(_).apply[F]
  }
}