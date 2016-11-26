package pavlosgi.freecli.core.dsl.config

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.OptDslBuilder.{DefaultValue, Required}

private[config] case class OptDslBuilder[H <: HList, T](list: H) {

  def --(
    name: String)
   (implicit ev: Prepend[H, FieldName :: HNil],
    ev2: NotContainsConstraint[H, FieldName]) =

    new OptDslBuilder[ev.Out, T](list :+ FieldName(name))

  def -(
    abbr: Char)
   (implicit ev: Prepend[H, FieldAbbreviation :: HNil],
    ev2: NotContainsConstraint[H, FieldAbbreviation]) =

    new OptDslBuilder[ev.Out, T](list :+ FieldAbbreviation(abbr))

  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    new OptDslBuilder[ev.Out, T](list :+ description)

  def -~(
    default: DefaultValue[T])
   (implicit ev: Prepend[H, DefaultValue[T] :: HNil],
    //TODO Make this constraint any DefaultValue[_]
    ev2: NotContainsConstraint[H, DefaultValue[T]]) =

    new OptDslBuilder[ev.Out, T](list :+ default)

  def -~(
    required: Required)
   (implicit ev: Prepend[H, Required :: HNil],
    ev2: NotContainsConstraint[H, Required]) =

    new OptDslBuilder[ev.Out, T](list :+ required)

}

private[config] object OptDslBuilder {
  case class DefaultValue[T](value: T)

  sealed trait Required
  def required = new Required {}

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

        FreeApplicative.lift(Opt[T, Option[T]](field, identity, decoder))
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

        FreeApplicative.lift(RequiredOpt[T, T](field, identity, decoder, Some(default)))
      }
    }

    implicit def canProduceConfigDslWithRequired[H <: HList, T, Out <: HList](
      implicit canProduceField: CanProduceField.Aux[H, Out],
      required: Out =:= (Required :: HNil),
      decoder: StringDecoder[T]):
      CanProduceConfigDsl[OptDslBuilder, H, T, T] = {

      (o: OptDslBuilder[H, T]) => {
        val (field, _) = canProduceField.apply(o.list)

        FreeApplicative.lift(RequiredOpt[T, T](field, identity, decoder, None))
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
    Merger[A] = {

    canProduceConfigDsl.apply(o)
  }
}