package pavlosgi.freecli.core.dsl.config

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._

private[config] case class FlagDslBuilder[H <: HList](list: H) {

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

private[config] object FlagDslBuilder {
  def flag: FlagDslBuilder[HNil] =
    new FlagDslBuilder[HNil](HNil)

  def flag[H <: HList](list: H): FlagDslBuilder[HNil] =
    new FlagDslBuilder[HNil](HNil)

  implicit def canProduceConfigDsl[H <: HList](
    implicit canProduceField: CanProduceField.Aux[H, HNil],
    decoder: StringDecoder[Boolean]):
    CanProduceConfigDsl[Lambda[T => FlagDslBuilder[H]], H, Boolean] = {

    (f: FlagDslBuilder[H]) => {
      val (field, _) = canProduceField.apply(f.list)

      FreeApplicative.lift(Flag[Boolean](field, identity))
    }
  }

  implicit def toConfigDsl[H <: HList](
    f: FlagDslBuilder[H])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[Lambda[T => FlagDslBuilder[H]], H, Boolean]):
    ConfigDsl[Boolean] = {

    canProduceConfigDsl(f)
  }

  implicit def toConfigDslMerger[H <: HList](
    f: FlagDslBuilder[H])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[Lambda[T => FlagDslBuilder[H]], H, Boolean]):
    Merger[Boolean] = {

    canProduceConfigDsl(f)
  }
}