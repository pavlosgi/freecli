package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._

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

  trait CanProduceConfigDsl[F[_ <: HList], H <: HList, A] {
    def apply(a: F[H]): ConfigDsl[A]
  }

  object CanProduceConfigDsl {
    implicit def canProduceConfigDsl[H <: HList](
      implicit canProduceField: CanProduceField.Aux[H, HNil],
      decoder: StringDecoder[Boolean]):
      CanProduceConfigDsl[FlagDslBuilder, H, Boolean] = {

      (f: FlagDslBuilder[H]) => {
        val (field, _) = canProduceField.apply(f.list)

        new ConfigDsl[Boolean] {
          override def apply[F[_] : Algebra]: F[Boolean] =
            implicitly[Algebra[F]].flag[Boolean](field, identity)

        }
      }
    }
  }

  implicit def toConfigDsl[H <: HList](
    f: FlagDslBuilder[H])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[FlagDslBuilder, H, Boolean]):
    ConfigDsl[Boolean] = {

    canProduceConfigDsl(f)
  }

  implicit def toConfigDslMerger[H <: HList](
    f: FlagDslBuilder[H])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[FlagDslBuilder, H, Boolean]):
    ConfigDsl.Merger[Boolean] = {

    canProduceConfigDsl(f)
  }

  implicit def builder2FA[H <: HList, F[_]: Algebra](
    implicit canProduceConfigDsl: CanProduceConfigDsl[FlagDslBuilder, H, Boolean]):
    FlagDslBuilder[H] => F[Boolean] = {

    canProduceConfigDsl(_).apply[F]
  }
}