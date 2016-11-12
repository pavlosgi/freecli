package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.config._

case class OptDslBuilder[H <: HList, T](list: H) extends Builder[H] {

  type Out[A <: HList] = OptDslBuilder[A, T]

  def --(
    name: String)
   (implicit ev: Prepend[H, FieldName :: HNil],
    ev2: NotContainsConstraint[H, FieldName]) =

    append(FieldName(name))

  def -(
    abbr: Char)
   (implicit ev: Prepend[H, FieldAbbreviation :: HNil],
    ev2: NotContainsConstraint[H, FieldAbbreviation]) =

    append(FieldAbbreviation(abbr))

  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    append(description)

  def -~(
    default: DefaultValue[T])
   (implicit ev: Prepend[H, DefaultValue[T] :: HNil],
    //TODO Make this constraint any DefaultValue[_]
    ev2: NotContainsConstraint[H, DefaultValue[T]]) =

    append(default)

  def -~(
    required: Required)
   (implicit ev: Prepend[H, Required :: HNil],
    ev2: NotContainsConstraint[H, Required]) =

    append(required)

  override def append[A](
   t: A)
  (implicit ev: Prepend[H, ::[A, HNil]]):
   OptDslBuilder[ev.Out, T] = {

   new OptDslBuilder(list :+ t)
  }
}

object OptDslBuilder {
  def opt[T](implicit decoder: StringDecoder[T]): OptDslBuilder[HNil, T] =
    opt(HNil)

  def opt[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new OptDslBuilder[H, T](list)

  trait CanProduceConfigDsl[F[_ <: HList, _], H <: HList, T, A] {
    def apply(a: F[H, T]): ConfigDsl[A]
  }

  object CanProduceConfigDsl {
    implicit def canProduceConfigDsl[H <: HList, T, Out <: HList](
      implicit canProduceField: CanProduceField.Aux[H, HNil],
      decoder: StringDecoder[T]):
      CanProduceConfigDsl[OptDslBuilder, H, T, Option[T]] = {

      (o: OptDslBuilder[H, T]) => {
        val (field, _) = canProduceField.apply(o.list)

        new ConfigDsl[Option[T]] {
          override def apply[F[_] : Algebra]: F[Option[T]] =
            implicitly[Algebra[F]].opt[T, Option[T]](field, identity, decoder)

        }
      }
    }

    implicit def canProduceConfigDslWithDefault[H <: HList, T, Out <: HList](
      implicit canProduceField: CanProduceField.Aux[H, Out],
      canProduceDefault: CanProduceDefault.Aux[T, Out, _],
      decoder: StringDecoder[T]):
      CanProduceConfigDsl[OptDslBuilder, H, T, T] = {

      (o: OptDslBuilder[H, T]) => {
        val (field, remaining) = canProduceField.apply(o.list)
        val (default, _) = canProduceDefault.apply(remaining)

        new ConfigDsl[T] {
          override def apply[F[_] : Algebra]: F[T] =
            implicitly[Algebra[F]].requiredOpt[T, T](
              field,
              identity,
              decoder,
              Some(default))
        }
      }
    }

    implicit def canProduceConfigDslWithRequired[H <: HList, T, Out <: HList](
      implicit canProduceField: CanProduceField.Aux[H, Out],
      required: Out =:= (Required :: HNil),
      decoder: StringDecoder[T]):
      CanProduceConfigDsl[OptDslBuilder, H, T, T] = {

      (o: OptDslBuilder[H, T]) => {
        val (field, _) = canProduceField.apply(o.list)

        new ConfigDsl[T] {
          override def apply[F[_] : Algebra]: F[T] =
            implicitly[Algebra[F]].requiredOpt[T, T](
              field,
              identity,
              decoder,
              None)
        }
      }
    }
  }

  implicit def toConfigDsl[H <: HList, T, A](
    o: OptDslBuilder[H, T])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[OptDslBuilder, H, T, A]):
    ConfigDsl[A] = {

    canProduceConfigDsl.apply(o)
  }

  implicit def toConfigDslMerger[H <: HList, T, A](
    o: OptDslBuilder[H, T])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[OptDslBuilder, H, T, A]):
    ConfigDsl.Merger[A] = {

    canProduceConfigDsl.apply(o)
  }

  implicit def builder2FA[H <: HList, T, A, Out <: HList, F[_]: Algebra](
    implicit canProduceConfigDsl: CanProduceConfigDsl[OptDslBuilder, H, T, A]):
    OptDslBuilder[H, T] => F[A] = {

    canProduceConfigDsl(_).apply[F]
  }
}