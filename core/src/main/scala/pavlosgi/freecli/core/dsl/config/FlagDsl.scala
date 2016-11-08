package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.DefaultBuilder.CanProduceDefault
import pavlosgi.freecli.core.dsl.config.FieldBuilder.CanProduceField

case class FlagDsl[H <: HList](list: H)
 extends FieldNameDslOps[H]
 with FieldDescriptionDslOps[H]
 with Builder[H] {

 type Out[A <: HList] = FlagDsl[A]

 def -~(
   default: DefaultValue[Boolean])
  (implicit ev: Prepend[H, DefaultValue[Boolean] :: HNil],
   //TODO Make this constraint any DefaultValue[_]
   ev2: NotContainsConstraint[H, DefaultValue[Boolean]]) =

  append(default)

 override def append[A](t: A)(implicit ev: Prepend[H, ::[A, HNil]]): FlagDsl[ev.Out] = {
   new FlagDsl(list :+ t)
 }
}

object FlagDsl {
  def flag: FlagDsl[HNil] =
    new FlagDsl[HNil](HNil)

  def flag[H <: HList](list: H): FlagDsl[HNil] =
    new FlagDsl[HNil](HNil)

  implicit def canProduceConfigDsl[H <: HList](
    implicit canProduceField: CanProduceField.Aux[H, HNil],
    decoder: StringDecoder[Boolean]):
    CanProduceConfigDslF[FlagDsl, H, Boolean] = {

    new CanProduceConfigDslF[FlagDsl, H, Boolean] {
      def apply(f: FlagDsl[H]): ConfigDsl[Boolean] = {
        val (field, _) = canProduceField.apply(f.list)

        new ConfigDsl[Boolean] {
          override def apply[F[_] : Algebra]: F[Boolean] =
            implicitly[Algebra[F]].flag[Boolean](field, identity)

        }
      }
    }
  }

  implicit def canProduceConfigDslWithDefault[H <: HList, Out <: HList](
    implicit canProduceField: CanProduceField.Aux[H, Out],
    canProduceDefault: CanProduceDefault.Aux[Boolean, Out, HNil],
    decoder: StringDecoder[Boolean]):
    CanProduceConfigDslF[FlagDsl, H, Boolean] = {

    new CanProduceConfigDslF[FlagDsl, H, Boolean] {
      def apply(f: FlagDsl[H]): ConfigDsl[Boolean] = {
        val (field, _) = canProduceField.apply(f.list)

        new ConfigDsl[Boolean] {
          override def apply[F[_] : Algebra]: F[Boolean] =
            implicitly[Algebra[F]].flag[Boolean](field, identity)

        }
      }
    }
  }

  implicit def toConfigDsl[H <: HList](
    f: FlagDsl[H])
   (implicit canProduceConfigDsl: CanProduceConfigDslF[FlagDsl, H, Boolean]):
    ConfigDsl[Boolean] = {

    canProduceConfigDsl(f)
  }

  implicit def toConfigDslMerger[H <: HList](
    f: FlagDsl[H])
   (implicit canProduceConfigDsl: CanProduceConfigDslF[FlagDsl, H, Boolean]):
    ConfigDsl.Merger[Boolean] = {

    canProduceConfigDsl(f)
  }

  implicit def dsl2FA[H <: HList, F[_]: Algebra](
    implicit canProduceConfigDsl: CanProduceConfigDslF[FlagDsl, H, Boolean]):
    FlagDsl[H] => F[Boolean] = {

    canProduceConfigDsl(_).apply[F]
  }
}