package pavlosgi.freecli.option.dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.{CanProduce, Description, StringDecoder}
import pavlosgi.freecli.option.api._
import pavlosgi.freecli.option.dsl.OptDslBuilder.{DefaultValue, Required}

case class OptDslBuilder[H <: HList, T](list: H) {

  def --(
    name: String)
   (implicit ev: Prepend[H, OptionFieldName :: HNil],
    ev2: NotContainsConstraint[H, OptionFieldName]) =

    new OptDslBuilder[ev.Out, T](list :+ OptionFieldName(name))

  def -(
    abbr: Char)
   (implicit ev: Prepend[H, OptionFieldAbbreviation :: HNil],
    ev2: NotContainsConstraint[H, OptionFieldAbbreviation]) =

    new OptDslBuilder[ev.Out, T](list :+ OptionFieldAbbreviation(abbr))

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
    implicit canProduceField: CanProduce.Aux[H, (OptionField, HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[OptDslBuilder[H, T], OptionDsl[Option[T]]] = {

    new CanProduce[OptDslBuilder[H, T]] {
      type Out = OptionDsl[Option[T]]
      def apply(o: OptDslBuilder[H, T]): Out = {
        val (field, _) = canProduceField.apply(o.list)
        FreeApplicative.lift(Opt[T, Option[T]](field, identity, decoder))
      }
    }
  }

  implicit def canProduceConfigDslWithDefault[H <: HList, T, Out <: HList](
    implicit canProduceField: CanProduce.Aux[H, (OptionField, Out)],
    canProduceDefault: CanProduce.Aux[Out, (DefaultValue[T], HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[OptDslBuilder[H, T], OptionDsl[T]] = {

    new CanProduce[OptDslBuilder[H, T]] {
      type Out = OptionDsl[T]
      def apply(o: OptDslBuilder[H, T]): Out = {
        val (field, remaining) = canProduceField.apply(o.list)
        val (default, _) = canProduceDefault.apply(remaining)

        FreeApplicative.lift(RequiredOpt[T, T](field, identity, decoder, Some(default.value)))
      }
    }
  }

  implicit def canProduceConfigDslWithDefaultWithRequired[H <: HList, T, Out <: HList](
    implicit canProduceField: CanProduce.Aux[H, (OptionField, Out)],
    canProduceDefault: CanProduce.Aux[Out, (DefaultValue[T], Required :: HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[OptDslBuilder[H, T], OptionDsl[T]] = {

    new CanProduce[OptDslBuilder[H, T]] {
      type Out = OptionDsl[T]
      def apply(o: OptDslBuilder[H, T]): Out = {
        val (field, remaining) = canProduceField.apply(o.list)
        val (default, _) = canProduceDefault.apply(remaining)

        FreeApplicative.lift(RequiredOpt[T, T](field, identity, decoder, Some(default.value)))
      }
    }
  }

  implicit def canProduceConfigDslWithRequired[H <: HList, T](
    implicit canProduceField: CanProduce.Aux[H, (OptionField, Required :: HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[OptDslBuilder[H, T], OptionDsl[T]] = {

    new CanProduce[OptDslBuilder[H, T]] {
      type Out = OptionDsl[T]
      def apply(o: OptDslBuilder[H, T]): Out = {
        val (field, _) = canProduceField.apply(o.list)
        FreeApplicative.lift(RequiredOpt[T, T](field, identity, decoder, None))
      }
    }
  }
}