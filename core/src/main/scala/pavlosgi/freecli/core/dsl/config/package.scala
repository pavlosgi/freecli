package pavlosgi.freecli.core.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.Tupler

import pavlosgi.freecli.core.api.config.{Description, FieldAbbreviation, FieldName, StringDecoder}

package object config {
  def config[T]: ConfigDsl.Apply[T] = new ConfigDsl.Apply[T]
  def config[T <: HList, Tup](
    c: ConfigDsl[T])
   (implicit ev: Tupler.Aux[T, Tup]):
    ConfigDsl[Tup] = c.map(ev.apply)

  def arg[T](implicit ev: StringDecoder[T]) = ArgDsl.arg

  def arg[T](
    fieldName: String,
    description: Option[String],
    default: Option[T])
   (implicit ev: StringDecoder[T]) =

    ArgDsl.arg[FieldName :: Option[Description] ::
      Option[DefaultValue[T]] :: HNil, T](

      FieldName(fieldName) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def arg[T](
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[T])
   (implicit ev: StringDecoder[T]) =

    ArgDsl.arg[FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[T]] :: HNil, T](

      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)


  def arg[T](
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[T])
   (implicit ev: StringDecoder[T]) =

    ArgDsl.arg[FieldName :: FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[T]] :: HNil, T](

      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: default.map(DefaultValue(_)) :: HNil)

  def string = ArgDsl.arg[String]

  def string(
    fieldName: String,
    description: Option[String],
    default: Option[String]) =

    ArgDsl.arg[FieldName :: Option[Description] ::
      Option[DefaultValue[String]] :: HNil, String](

      FieldName(fieldName) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def string(
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[String]) =

    ArgDsl.arg[FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[String]] :: HNil, String](

      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def string(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[String]) =

    ArgDsl.arg[FieldName :: FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[String]] :: HNil, String](

      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: default.map(DefaultValue(_)) :: HNil)

  def int = ArgDsl.arg[Int]

  def int(
    fieldName: String,
    description: Option[String],
    default: Option[Int]) =

    ArgDsl.arg[FieldName :: Option[Description] ::
      Option[DefaultValue[Int]] :: HNil, Int](

      FieldName(fieldName) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def int(
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[Int]) =

    ArgDsl.arg[FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[Int]] :: HNil, Int](

      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def int(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[Int]) =

    ArgDsl.arg[FieldName :: FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[Int]] :: HNil, Int](

      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: default.map(DefaultValue(_)) :: HNil)

  def flag(implicit ev: StringDecoder[Boolean]) = FlagDsl.flag

  def flag(
    fieldName: String,
    description: Option[String],
    default: Option[Boolean])
   (implicit ev: StringDecoder[Boolean]) =

    FlagDsl.flag[FieldName :: Option[Description] ::
      Option[DefaultValue[Boolean]] :: HNil](

      FieldName(fieldName) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def flag(
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[Boolean])
   (implicit ev: StringDecoder[Boolean]) =

    FlagDsl.flag[FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[Boolean]] :: HNil](

      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def flag(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[Boolean])
   (implicit ev: StringDecoder[Boolean]) =

    FlagDsl.flag[FieldName :: FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[Boolean]] :: HNil](

      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: default.map(DefaultValue(_)) :: HNil)

  def boolean = FlagDsl.flag

  def opt[T](implicit ev: StringDecoder[T]) = OptDsl.opt[T]

  def opt[T](
    fieldName: String,
    description: Option[String])
   (implicit ev: StringDecoder[T]) =

    OptDsl.opt[FieldName :: Option[Description] :: HNil, T](
      FieldName(fieldName) :: description.map(Description.apply) :: HNil)

  def opt[T](
    fieldAbbreviation: Char,
    description: Option[String])
   (implicit ev: StringDecoder[T]) =

    OptDsl.opt[FieldAbbreviation :: Option[Description] :: HNil, T](
      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) :: HNil)

  def opt[T](
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String])
   (implicit ev: StringDecoder[T]) =

    OptDsl.opt[FieldName :: FieldAbbreviation :: Option[Description] :: HNil, T](
      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: HNil)

  def optString = OptDsl.opt[String]

  def optString(
    fieldName: String,
    description: Option[String]) =

    OptDsl.opt[FieldName :: Option[Description] :: HNil, String](
      FieldName(fieldName) :: description.map(Description.apply) :: HNil)

  def optString(
    fieldAbbreviation: Char,
    description: Option[String]) =

    OptDsl.opt[FieldAbbreviation :: Option[Description] :: HNil, String](
      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) :: HNil)

  def optString(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String]) =

    OptDsl.opt[FieldName :: FieldAbbreviation :: Option[Description] :: HNil, String](
      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: HNil)

  def optInt = OptDsl.opt[Int]

  def optInt(
    fieldName: String,
    description: Option[String]) =

    OptDsl.opt[FieldName :: Option[Description] :: HNil, Int](
      FieldName(fieldName) :: description.map(Description.apply) :: HNil)

  def optInt(
    fieldAbbreviation: Char,
    description: Option[String]) =

    OptDsl.opt[FieldAbbreviation :: Option[Description] :: HNil, Int](
      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) :: HNil)

  def optInt(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String]) =

    OptDsl.opt[FieldName :: FieldAbbreviation :: Option[Description] :: HNil, Int](
      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: HNil)

  def sub[T] = SubDsl.sub[T]

  def sub[T](description: String) =
    SubDsl.sub[Description :: HNil, T](Description(description) :: HNil)

}
