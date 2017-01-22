package pavlosgi.freecli.option.dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.{CanProduce, Description}
import pavlosgi.freecli.option.api._

case class VersionDslBuilder[H <: HList](list: H) {

  def --(
    name: String)
   (implicit ev: Prepend[H, OptionFieldName :: HNil],
    ev2: NotContainsConstraint[H, OptionFieldName]) =

    new VersionDslBuilder(list :+ OptionFieldName(name))

  def -(
    abbr: Char)
   (implicit ev: Prepend[H, OptionFieldAbbreviation :: HNil],
    ev2: NotContainsConstraint[H, OptionFieldAbbreviation]) =

    new VersionDslBuilder(list :+ OptionFieldAbbreviation(abbr))

  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    new VersionDslBuilder(list :+ description)

  def -~(
    value: StringValue)
   (implicit ev: Prepend[H, StringValue :: HNil],
    ev2: NotContainsConstraint[H, StringValue]) =

    new VersionDslBuilder(list :+ value)

}

object VersionDslBuilder {
  def version: VersionDslBuilder[HNil] =
    new VersionDslBuilder[HNil](HNil)

  implicit def canProduceOptionDsl[H <: HList, L <: HList](
    implicit canProduceField: CanProduce.Aux[H, (OptionField, L)],
    canProduceStringValue: CanProduce.Aux[L, (StringValue, HNil)]):
    CanProduce.Aux[VersionDslBuilder[H], OptionDsl[HNil]] = {
    new CanProduce[VersionDslBuilder[H]] {
      type Out = OptionDsl[HNil]
      def apply(t: VersionDslBuilder[H]): Out = {
        val (field, rem) = canProduceField.apply(t.list)
        val (value, _) = canProduceStringValue.apply(rem)
        FreeApplicative.lift(Version[HNil](field, value, identity))
      }
    }
  }
}