package pavlosgi.freecli.core.dsl.options

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.{Description, StringDecoder}
import pavlosgi.freecli.core.api.options._
import pavlosgi.freecli.core.dsl.CanProduce
import pavlosgi.freecli.core.dsl.options.OptDslBuilder.{DefaultValue, Required}

case class OptDslBuilder[H <: HList, T](list: H) {

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

object OptDslBuilder {
  case class DefaultValue[T](value: T)

  sealed trait Required
  def required = new Required {}

  def opt[T](implicit decoder: StringDecoder[T]): OptDslBuilder[HNil, T] =
    opt(HNil)

  def opt[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new OptDslBuilder[H, T](list)

  implicit def canProduceConfigDsl[H <: HList, T, Out <: HList](
    implicit canProduceField: CanProduce.Aux[H, (Field, HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[OptDslBuilder[H, T], OptionsDsl[Option[T]]] = {

    new CanProduce[OptDslBuilder[H, T]] {
      type Out = OptionsDsl[Option[T]]
      def apply(o: OptDslBuilder[H, T]): Out = {
        val (field, _) = canProduceField.apply(o.list)
        FreeApplicative.lift(Opt[T, Option[T]](field, identity, decoder))
      }
    }
  }

  implicit def canProduceConfigDslWithDefault[H <: HList, T, Out <: HList](
    implicit canProduceField: CanProduce.Aux[H, (Field, Out)],
    canProduceDefault: CanProduce.Aux[Out, (DefaultValue[T], HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[OptDslBuilder[H, T], OptionsDsl[T]] = {

    new CanProduce[OptDslBuilder[H, T]] {
      type Out = OptionsDsl[T]
      def apply(o: OptDslBuilder[H, T]): Out = {
        val (field, remaining) = canProduceField.apply(o.list)
        val (default, _) = canProduceDefault.apply(remaining)

        FreeApplicative.lift(RequiredOpt[T, T](field, identity, decoder, Some(default.value)))
      }
    }
  }

  implicit def canProduceConfigDslWithDefaultWithRequired[H <: HList, T, Out <: HList](
    implicit canProduceField: CanProduce.Aux[H, (Field, Out)],
    canProduceDefault: CanProduce.Aux[Out, (DefaultValue[T], Required :: HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[OptDslBuilder[H, T], OptionsDsl[T]] = {

    new CanProduce[OptDslBuilder[H, T]] {
      type Out = OptionsDsl[T]
      def apply(o: OptDslBuilder[H, T]): Out = {
        val (field, remaining) = canProduceField.apply(o.list)
        val (default, _) = canProduceDefault.apply(remaining)

        FreeApplicative.lift(RequiredOpt[T, T](field, identity, decoder, Some(default.value)))
      }
    }
  }

  implicit def canProduceConfigDslWithRequired[H <: HList, T](
    implicit canProduceField: CanProduce.Aux[H, (Field, Required :: HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[OptDslBuilder[H, T], OptionsDsl[T]] = {

    new CanProduce[OptDslBuilder[H, T]] {
      type Out = OptionsDsl[T]
      def apply(o: OptDslBuilder[H, T]): Out = {
        val (field, _) = canProduceField.apply(o.list)
        FreeApplicative.lift(RequiredOpt[T, T](field, identity, decoder, None))
      }
    }
  }
}