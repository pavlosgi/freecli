package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.config._

case class FlagDsl[H <: HList](list: H) extends Builder[H] {

  type Out[A <: HList] = FlagDsl[A]

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

  override def append[A](t: A)(implicit ev: Prepend[H, ::[A, HNil]]): FlagDsl[ev.Out] = {
    new FlagDsl(list :+ t)
  }
}

object FlagDsl {
  def flag: FlagDsl[HNil] =
    new FlagDsl[HNil](HNil)

  def flag[H <: HList](list: H): FlagDsl[HNil] =
    new FlagDsl[HNil](HNil)

  trait CanProduceConfigDsl[F[_ <: HList], H <: HList, A] {
    def apply(a: F[H]): ConfigDsl[A]
  }

  object CanProduceConfigDsl {
    implicit def canProduceConfigDsl[H <: HList](
      implicit canProduceField: CanProduceField.Aux[H, HNil],
      decoder: StringDecoder[Boolean]):
      CanProduceConfigDsl[FlagDsl, H, Boolean] = {

      new CanProduceConfigDsl[FlagDsl, H, Boolean] {
        def apply(f: FlagDsl[H]): ConfigDsl[Boolean] = {
          val (field, _) = canProduceField.apply(f.list)

          new ConfigDsl[Boolean] {
            override def apply[F[_] : Algebra]: F[Boolean] =
              implicitly[Algebra[F]].flag[Boolean](field, identity)

          }
        }
      }
    }
  }

  implicit def toConfigDsl[H <: HList](
    f: FlagDsl[H])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[FlagDsl, H, Boolean]):
    ConfigDsl[Boolean] = {

    canProduceConfigDsl(f)
  }

  implicit def toConfigDslMerger[H <: HList](
    f: FlagDsl[H])
   (implicit canProduceConfigDsl: CanProduceConfigDsl[FlagDsl, H, Boolean]):
    ConfigDsl.Merger[Boolean] = {

    canProduceConfigDsl(f)
  }

  implicit def dsl2FA[H <: HList, F[_]: Algebra](
    implicit canProduceConfigDsl: CanProduceConfigDsl[FlagDsl, H, Boolean]):
    FlagDsl[H] => F[Boolean] = {

    canProduceConfigDsl(_).apply[F]
  }
}