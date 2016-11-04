package pavlosgi.freecli.core.dsl.config

import scala.annotation.implicitNotFound

import cats.Applicative
import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.{Prepend, Selector}

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.DefaultBuilder.CanProduceDefault
import pavlosgi.freecli.core.dsl.config.DescriptionBuilder.CanProduceDescription
import pavlosgi.freecli.core.dsl.config.FieldBuilder.CanProduceField
import PartialConfigDsl._

private[config] class PartialConfigDsl[H <: HList, AO <: AlgebraOperation, T] private[config](
  val list: H) {

  def --(
    name: String)
   (implicit ev: Prepend[H, FieldName :: HNil],
    ev2: NotContainsConstraint[H, FieldName],
    ev3: IsSupported[H, AO, FieldName]):
    PartialConfigDsl[ev.Out, AO, T] =

    new PartialConfigDsl[ev.Out, AO, T](this.list :+ FieldName(name))

  def -(
    abbr: Char)
   (implicit ev: Prepend[H, FieldAbbreviation :: HNil],
    ev2: NotContainsConstraint[H, FieldAbbreviation],
    ev3: IsSupported[H, AO, FieldAbbreviation]):
    PartialConfigDsl[ev.Out, AO, T] =

    new PartialConfigDsl[ev.Out, AO, T](list :+ FieldAbbreviation(abbr))

  def -?(
    description: String)
   (implicit ev: Prepend[H, Option[Description] :: HNil],
    ev2: NotContainsConstraint[H, Option[Description]],
    ev3: IsSupported[H, AO, Option[Description]]):
    PartialConfigDsl[ev.Out, AO, T] =

    new PartialConfigDsl[ev.Out, AO, T](this.list :+ Option(Description(description)))

  def -|(
    default: T)
   (implicit ev: Prepend[H, DefaultValue[T] :: HNil],
    ev2: NotContainsConstraint[H, DefaultValue[_]],
    ev3: IsSupported[H, AO, DefaultValue[T]]):
    PartialConfigDsl[ev.Out, AO, T] =

    new PartialConfigDsl[ev.Out, AO, T](this.list :+ DefaultValue(default))

  def apply[L <: HList](
    dsl: ConfigDsl[L])
   (implicit ev: Prepend[H, ConfigDsl[L] :: HNil],
    ev2: NotContainsConstraint[H, ConfigDsl[_]],
    ev3: Selector[H, Description],
    ev4: IsSupported[H, AO, ConfigDsl[L]]):
    PartialConfigDsl[ev.Out, AO, T] = {

    new PartialConfigDsl(list :+ dsl)
  }
}

object PartialConfigDsl {

  def flag: PartialConfigDsl[HNil, Flag, Boolean] =
    new PartialConfigDsl[HNil, Flag, Boolean](HNil)

  def flag[H <: HList](list: H): PartialConfigDsl[HNil, Flag, Boolean] =
    new PartialConfigDsl[HNil, Flag, Boolean](HNil)

  def arg[T](implicit decoder: StringDecoder[T]): PartialConfigDsl[HNil, Arg, T] = arg(HNil)

  def arg[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new PartialConfigDsl[H, Arg, T](list)

  def opt[T](implicit decoder: StringDecoder[T]): PartialConfigDsl[HNil, Opt, Option[T]] =
    opt(HNil)

  def opt[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new PartialConfigDsl[H, Opt, Option[T]](list)

  def sub[T]: PartialConfigDsl[HNil, Sub, T] = sub[HNil, T](HNil)
  def sub[H <: HList, T](list: H): PartialConfigDsl[H, Sub, T] =
    new PartialConfigDsl[H, Sub, T](list)

  @implicitNotFound(s"Could not find IsSupported[$${H}, $${AO}, $${T}]. Please ensure " +
                    "that your can use this operator in $${AO}")
  sealed trait IsSupported[H <: HList, AO <: AlgebraOperation, T]
  sealed trait ConfigDslConverter[H <: HList, T, AO <: AlgebraOperation] {
    def configDsl(argDsl: PartialConfigDsl[H, AO, T]): ConfigDsl[T]
  }

  sealed trait AlgebraOperation
  sealed trait Flag extends AlgebraOperation
  object Flag {
    implicit def canUseFieldName[H <: HList] =
      new IsSupported[H, Flag, FieldName] {}

    implicit def canUseFieldAbbreviation[H <: HList] =
      new IsSupported[H, Flag, FieldAbbreviation] {}

    implicit def canUseDescription[H <: HList] =
      new IsSupported[H, Flag, Option[Description]] {}

    implicit def canUseDefault[H <: HList, T] =
      new IsSupported[H, Flag, DefaultValue[T]] {}

    implicit def configDslConverter[H <: HList, FOut <: HList]
     (implicit canProduceField: CanProduceField.Aux[H, FOut],
      canProduceDefault: CanProduceDefault[Boolean, FOut]):
      ConfigDslConverter[H, Boolean, Flag] =

      new ConfigDslConverter[H, Boolean, Flag] {
        override def configDsl(
          f: PartialConfigDsl[H, Flag, Boolean]):
          ConfigDsl[Boolean] = {

          val (field, remaining) = canProduceField.apply(f.list)
          val (default, _) = canProduceDefault.apply(remaining)

          new ConfigDsl[Boolean] {
            override def apply[F[_] : Algebra]: F[Boolean] =
              implicitly[Algebra[F]].flag[Boolean](field, identity, default)

          }
        }
      }
  }

  sealed trait Arg extends AlgebraOperation
  object Arg {
    implicit def canUseFieldName[H <: HList] =
      new IsSupported[H, Arg, FieldName] {}

    implicit def canUseFieldAbbreviation[H <: HList] =
      new IsSupported[H, Arg, FieldAbbreviation] {}

    implicit def canUseDescription[H <: HList] =
      new IsSupported[H, Arg, Option[Description]] {}

    implicit def canUseDefault[H <: HList, T] =
      new IsSupported[H, Arg, DefaultValue[T]] {}

    implicit def configDslConverter[T, H <: HList, FOut <: HList]
     (implicit canProduceField: CanProduceField.Aux[H, FOut],
      canProduceDefault: CanProduceDefault[T, FOut],
      decoder: StringDecoder[T]):
      ConfigDslConverter[H, T, Arg] =

      new ConfigDslConverter[H, T , Arg] {
        override def configDsl(f: PartialConfigDsl[H, Arg, T]): ConfigDsl[T] = {
          val (field, remaining) = canProduceField.apply(f.list)
          val (default, _) = canProduceDefault.apply(remaining)

          new ConfigDsl[T] {
            override def apply[F[_] : Algebra]: F[T] =
              implicitly[Algebra[F]].arg[T, T](field, identity, decoder, default)

          }
        }
      }
  }

  sealed trait Opt extends AlgebraOperation
  object Opt {
    implicit def canUseFieldName[H <: HList] =
      new IsSupported[H, Opt, FieldName] {}

    implicit def canUseFieldAbbreviation[H <: HList] =
      new IsSupported[H, Opt, FieldAbbreviation] {}

    implicit def canUseDescription[H <: HList] =
      new IsSupported[H, Opt, Option[Description]] {}

    implicit def configDslConverter[T, H <: HList, FOut <: HList]
     (implicit canProduceField: CanProduceField.Aux[H, FOut],
      decoder: StringDecoder[T]):
      ConfigDslConverter[H, Option[T], Opt] =

      new ConfigDslConverter[H, Option[T], Opt] {
        override def configDsl(f: PartialConfigDsl[H, Opt, Option[T]]): ConfigDsl[Option[T]] = {
          val (field, _) = canProduceField.apply(f.list)

          new ConfigDsl[Option[T]] {
            override def apply[F[_] : Algebra]: F[Option[T]] =
              implicitly[Algebra[F]].opt[T, Option[T]](field, identity, decoder)

          }
        }
      }
  }

  sealed trait Sub extends AlgebraOperation
  object Sub {
    implicit def canUseConfigDsl[H <: HList, T] =
      new IsSupported[H, Sub, ConfigDsl[T]] {}

    implicit def configDslConverter[B, H <: HList, DOut <: HList, L <: HList]
     (implicit gen: Generic.Aux[B, L],
      canProduceDescription: CanProduceDescription.Aux[H, DOut],
      selector: Selector[DOut, ConfigDsl[L]]):
      ConfigDslConverter[H, B, Sub] =

      new ConfigDslConverter[H, B, Sub] {
        override def configDsl(f: PartialConfigDsl[H, Sub, B]): ConfigDsl[B] = {
          val (description, remaining) = canProduceDescription.apply(f.list)

          val subDsl: ConfigDsl[B] =
            implicitly[Applicative[ConfigDsl]].map(selector.apply(remaining))(gen.from)

          new ConfigDsl[B] {
            override def apply[F[_] : Algebra]: F[B] =
              implicitly[Algebra[F]].sub(description, subDsl)

          }
        }
      }
  }

  implicit def partial2F[T, AO <: AlgebraOperation, H <: HList, F[_]: Algebra]
   (implicit ev: ConfigDslConverter[H, T, AO]):
    PartialConfigDsl[H, AO, T] => F[T] =

    ev.configDsl(_).apply[F]

  implicit def toConfigDslList[T, AO <: AlgebraOperation, H <: HList](
   argDsl: PartialConfigDsl[H, AO, T])
  (implicit ev: ConfigDslConverter[H, T, AO]): ConfigDsl[T :: HNil] =
    ev.configDsl(argDsl).map(_ :: HNil)

  implicit def toConfigDslMerger[T, AO <: AlgebraOperation, H <: HList](
   argDsl: PartialConfigDsl[H, AO, T])
  (implicit ev: ConfigDslConverter[H, T, AO]): ConfigDsl.Merger[T :: HNil] =
    toConfigDslList(argDsl)
}