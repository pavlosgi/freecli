package pavlosgi.freecli.core.dsl.options

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.options._

case class FlagDslBuilder[H <: HList](list: H) extends Builder[H] {

  type Out[A <: HList] = FlagDslBuilder[A]

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

  override def append[A](t: A)(implicit ev: Prepend[H, ::[A, HNil]]): FlagDslBuilder[ev.Out] = {
    new FlagDslBuilder(list :+ t)
  }
}

object FlagDslBuilder {
  def flag: FlagDslBuilder[HNil] =
    new FlagDslBuilder[HNil](HNil)

  def flag[H <: HList](list: H): FlagDslBuilder[HNil] =
    new FlagDslBuilder[HNil](HNil)

  trait CanProduceOptionsDsl[F[_ <: HList], H <: HList, A] {
    def apply(a: F[H]): OptionsDsl[A]
  }

  object CanProduceOptionsDsl {
    implicit def canProduceOptionsDsl[H <: HList](
      implicit canProduceField: CanProduceField.Aux[H, HNil],
      decoder: StringDecoder[Boolean]):
      CanProduceOptionsDsl[FlagDslBuilder, H, Boolean] = {

      (f: FlagDslBuilder[H]) => {
        val (field, _) = canProduceField.apply(f.list)

        new OptionsDsl[Boolean] {
          override def apply[F[_] : Algebra]: F[Boolean] =
            implicitly[Algebra[F]].flag[Boolean](field, identity)

        }
      }
    }
  }

  implicit def toOptionsDsl[H <: HList](
    f: FlagDslBuilder[H])
   (implicit canProduceOptionsDsl: CanProduceOptionsDsl[FlagDslBuilder, H, Boolean]):
    OptionsDsl[Boolean] = {

    canProduceOptionsDsl(f)
  }

  implicit def toOptionsDslMerger[H <: HList](
    f: FlagDslBuilder[H])
   (implicit canProduceOptionsDsl: CanProduceOptionsDsl[FlagDslBuilder, H, Boolean]):
    OptionsDsl.Merger[Boolean] = {

    canProduceOptionsDsl(f)
  }

  implicit def builder2FA[H <: HList, F[_]: Algebra](
    implicit canProduceOptionsDsl: CanProduceOptionsDsl[FlagDslBuilder, H, Boolean]):
    FlagDslBuilder[H] => F[Boolean] = {

    canProduceOptionsDsl(_).apply[F]
  }
}