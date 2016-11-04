package pavlosgi.freecli.core.config

import shapeless._

import pavlosgi.freecli.core.api.config.{Description, FieldAbbreviation, FieldName, StringDecoder}

package object dsl {
  def config[B]: ConfigDsl.Builder[B] = new ConfigDsl.Builder[B]

  def arg[T](implicit ev: StringDecoder[T]) = PartialConfigDsl.arg

  def arg[T](
    fieldName: String,
    description: Option[String],
    default: Option[T])
   (implicit ev: StringDecoder[T]) =

    PartialConfigDsl.arg[FieldName :: Option[Description] ::
      Option[DefaultValue[T]] :: HNil, T](

      FieldName(fieldName) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def arg[T](
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[T])
   (implicit ev: StringDecoder[T]) =

    PartialConfigDsl.arg[FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[T]] :: HNil, T](

      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)


  def arg[T](
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[T])
   (implicit ev: StringDecoder[T]) =

    PartialConfigDsl.arg[FieldName :: FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[T]] :: HNil, T](

      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: default.map(DefaultValue(_)) :: HNil)

  def string = PartialConfigDsl.arg[String]

  def string(
    fieldName: String,
    description: Option[String],
    default: Option[String]) =

    PartialConfigDsl.arg[FieldName :: Option[Description] ::
      Option[DefaultValue[String]] :: HNil, String](

      FieldName(fieldName) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def string(
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[String]) =

    PartialConfigDsl.arg[FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[String]] :: HNil, String](

      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def string(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[String]) =

    PartialConfigDsl.arg[FieldName :: FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[String]] :: HNil, String](

      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: default.map(DefaultValue(_)) :: HNil)

  def int = PartialConfigDsl.arg[Int]

  def int(
    fieldName: String,
    description: Option[String],
    default: Option[Int]) =

    PartialConfigDsl.arg[FieldName :: Option[Description] ::
      Option[DefaultValue[Int]] :: HNil, Int](

      FieldName(fieldName) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def int(
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[Int]) =

    PartialConfigDsl.arg[FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[Int]] :: HNil, Int](

      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def int(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[Int]) =

    PartialConfigDsl.arg[FieldName :: FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[Int]] :: HNil, Int](

      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: default.map(DefaultValue(_)) :: HNil)

  def flag(implicit ev: StringDecoder[Boolean]) = PartialConfigDsl.flag

  def flag(
    fieldName: String,
    description: Option[String],
    default: Option[Boolean])
   (implicit ev: StringDecoder[Boolean]) =

    PartialConfigDsl.flag[FieldName :: Option[Description] ::
      Option[DefaultValue[Boolean]] :: HNil](

      FieldName(fieldName) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def flag(
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[Boolean])
   (implicit ev: StringDecoder[Boolean]) =

    PartialConfigDsl.flag[FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[Boolean]] :: HNil](

      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) ::
        default.map(DefaultValue(_)) :: HNil)

  def flag(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String],
    default: Option[Boolean])
   (implicit ev: StringDecoder[Boolean]) =

    PartialConfigDsl.flag[FieldName :: FieldAbbreviation :: Option[Description] ::
      Option[DefaultValue[Boolean]] :: HNil](

      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: default.map(DefaultValue(_)) :: HNil)

  def boolean = PartialConfigDsl.flag

  def opt[T](implicit ev: StringDecoder[T]) = PartialConfigDsl.opt[T]

  def opt[T](
    fieldName: String,
    description: Option[String])
   (implicit ev: StringDecoder[T]) =

    PartialConfigDsl.opt[FieldName :: Option[Description] :: HNil, T](
      FieldName(fieldName) :: description.map(Description.apply) :: HNil)

  def opt[T](
    fieldAbbreviation: Char,
    description: Option[String])
   (implicit ev: StringDecoder[T]) =

    PartialConfigDsl.opt[FieldAbbreviation :: Option[Description] :: HNil, T](
      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) :: HNil)

  def opt[T](
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String])
   (implicit ev: StringDecoder[T]) =

    PartialConfigDsl.opt[FieldName :: FieldAbbreviation :: Option[Description] :: HNil, T](
      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: HNil)

  def optString = PartialConfigDsl.opt[String]

  def optString(
    fieldName: String,
    description: Option[String]) =

    PartialConfigDsl.opt[FieldName :: Option[Description] :: HNil, String](
      FieldName(fieldName) :: description.map(Description.apply) :: HNil)

  def optString(
    fieldAbbreviation: Char,
    description: Option[String]) =

    PartialConfigDsl.opt[FieldAbbreviation :: Option[Description] :: HNil, String](
      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) :: HNil)

  def optString(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String]) =

    PartialConfigDsl.opt[FieldName :: FieldAbbreviation :: Option[Description] :: HNil, String](
      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: HNil)

  def optInt = PartialConfigDsl.opt[Int]

  def optInt(
    fieldName: String,
    description: Option[String]) =

    PartialConfigDsl.opt[FieldName :: Option[Description] :: HNil, Int](
      FieldName(fieldName) :: description.map(Description.apply) :: HNil)

  def optInt(
    fieldAbbreviation: Char,
    description: Option[String]) =

    PartialConfigDsl.opt[FieldAbbreviation :: Option[Description] :: HNil, Int](
      FieldAbbreviation(fieldAbbreviation) :: description.map(Description.apply) :: HNil)

  def optInt(
    fieldName: String,
    fieldAbbreviation: Char,
    description: Option[String]) =

    PartialConfigDsl.opt[FieldName :: FieldAbbreviation :: Option[Description] :: HNil, Int](
      FieldName(fieldName) :: FieldAbbreviation(fieldAbbreviation) ::
        description.map(Description.apply) :: HNil)

  def sub[T] = PartialConfigDsl.sub[T]

  def sub[T](description: String) =
    PartialConfigDsl.sub[Description :: HNil, T](Description(description) :: HNil)

}
