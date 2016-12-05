package pavlosgi.freecli.core.dsl.options

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.{Description, StringDecoder}
import pavlosgi.freecli.core.api.options._
import pavlosgi.freecli.core.dsl.CanProduce

case class FlagDslBuilder[H <: HList](list: H) {

  def --(
    name: String)
   (implicit ev: Prepend[H, FieldName :: HNil],
    ev2: NotContainsConstraint[H, FieldName]) =

    new FlagDslBuilder(list :+ FieldName(name))

  def -(
    abbr: Char)
   (implicit ev: Prepend[H, FieldAbbreviation :: HNil],
    ev2: NotContainsConstraint[H, FieldAbbreviation]) =

    new FlagDslBuilder(list :+ FieldAbbreviation(abbr))

  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    new FlagDslBuilder(list :+ description)

}

object FlagDslBuilder {
  def flag: FlagDslBuilder[HNil] =
    new FlagDslBuilder[HNil](HNil)

  def flag[H <: HList](list: H): FlagDslBuilder[HNil] =
    new FlagDslBuilder[HNil](HNil)

  implicit def canProduceOptionsDsl[H <: HList](
    implicit canProduceField: CanProduce.Aux[H, (Field, HNil)],
    decoder: StringDecoder[Boolean]):
    CanProduce.Aux[FlagDslBuilder[H], OptionsDsl[Boolean]] = {

    new CanProduce[FlagDslBuilder[H]] {
      type Out = OptionsDsl[Boolean]
      def apply(t: FlagDslBuilder[H]): Out = {
        val (field, _) = canProduceField.apply(t.list)
        FreeApplicative.lift(Flag[Boolean](field, identity))
      }
    }
  }
}