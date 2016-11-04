package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.DefaultBuilder.CanProduceDefault
import pavlosgi.freecli.core.dsl.config.FieldBuilder.CanProduceField

case class FlagDsl[H <: HList](list: H)
 extends FieldNameOps[H]
 with FieldDescriptionOps[H]
 with DefaultOps[H, Boolean]
 with Builder[H] {

 type Out[A <: HList] = FlagDsl[A]

 override def append[A](t: A)(implicit ev: Prepend[H, ::[A, HNil]]): FlagDsl[ev.Out] = {
   new FlagDsl(list :+ t)
 }
}

object FlagDsl {
  def flag: FlagDsl[HNil] =
    new FlagDsl[HNil](HNil)

  def flag[H <: HList](list: H): FlagDsl[HNil] =
    new FlagDsl[HNil](HNil)

  implicit def toConfigDsl[H <: HList, Out <: HList](
    f: FlagDsl[H])
   (implicit canProduceField: CanProduceField.Aux[H, Out],
    canProduceDefault: CanProduceDefault[Boolean, Out]):
    ConfigDsl[Boolean] = {

    val (field, remaining) = canProduceField.apply(f.list)
    val (default, _) = canProduceDefault.apply(remaining)

    new ConfigDsl[Boolean] {
      override def apply[F[_] : Algebra]: F[Boolean] =
        implicitly[Algebra[F]].flag[Boolean](field, identity, default)

    }
  }

  implicit def toConfigDslMerger[H <: HList, Out <: HList](
    f: FlagDsl[H])
   (implicit canProduceField: CanProduceField.Aux[H, Out],
    canProduceDefault: CanProduceDefault[Boolean, Out]):
    ConfigDsl.Merger[Boolean] = {

    toConfigDsl(f)
  }

  implicit def dsl2FA[H <: HList, Out <: HList, F[_]: Algebra](
    implicit canProduceField: CanProduceField.Aux[H, Out],
    canProduceDefault: CanProduceDefault[Boolean, Out]):
    FlagDsl[H] => F[Boolean] = {

    toConfigDsl(_).apply[F]
  }

}