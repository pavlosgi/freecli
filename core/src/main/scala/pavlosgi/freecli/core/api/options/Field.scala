package pavlosgi.freecli.core.api.options

import cats.Show
import cats.instances.all._
import cats.syntax.all._

sealed trait Field {
  def description: Option[Description]
  def matches(s: String): Boolean
}

object Field {
  implicit def showInstance: Show[Field] = new Show[Field] {
    override def show(f: Field): String = {
      f match {
        case FieldNameOnly(name, description) =>
          name.show + description.fold("")(d => " " + d)

        case FieldAbbreviationOnly(abbr, description) =>
          abbr.show + description.fold("")(d => " " + d)

        case FieldNameAndAbbreviation(name, abbr, description) =>
          name.show + ", " + abbr.show +
            description.fold("")(d => " " + d)
      }
    }
  }

  def withFieldName(field: Field, name: FieldName): Field = {
    field match {
      case FieldNameOnly(_, des) => FieldNameOnly(name, des)
      case FieldAbbreviationOnly(abbr, des) =>
        FieldNameAndAbbreviation(name, abbr, des)

      case FieldNameAndAbbreviation(_, abbr, des) =>
        FieldNameAndAbbreviation(name, abbr, des)
    }
  }

  def withFieldAbbreviation(field: Field, abbr: FieldAbbreviation): Field = {
    field match {
      case FieldNameOnly(name, des) => FieldNameAndAbbreviation(name, abbr, des)
      case FieldAbbreviationOnly(_, des) =>
        FieldAbbreviationOnly(abbr, des)

      case FieldNameAndAbbreviation(name, _, des) =>
        FieldNameAndAbbreviation(name, abbr, des)
    }
  }

  def withDescription(field: Field, description: Description): Field = {
    field match {
      case FieldNameOnly(name, _) => FieldNameOnly(name, Some(description))
      case FieldAbbreviationOnly(abbr, _) =>
        FieldAbbreviationOnly(abbr, Some(description))

      case FieldNameAndAbbreviation(name, abbr, _) =>
        FieldNameAndAbbreviation(name, abbr, Some(description))
    }
  }

  def withOptionalDescription(field: Field, description: Option[Description]): Field = {
    field match {
      case FieldNameOnly(name, _) => FieldNameOnly(name, description)
      case FieldAbbreviationOnly(abbr, _) =>
        FieldAbbreviationOnly(abbr, description)

      case FieldNameAndAbbreviation(name, abbr, _) =>
        FieldNameAndAbbreviation(name, abbr, description)
    }
  }
}

case class FieldNameOnly(
  fieldName: FieldName,
  description: Option[Description]
) extends Field {
  override def matches(s: String): Boolean = fieldName.show === s
}

case class FieldAbbreviationOnly(
  fieldAbbreviation: FieldAbbreviation,
  description: Option[Description]
) extends Field {
  override def matches(s: String): Boolean = fieldAbbreviation.show === s
}

case class FieldNameAndAbbreviation(
  fieldName: FieldName,
  fieldAbbreviation: FieldAbbreviation,
  description: Option[Description]
) extends Field {
  override def matches(s: String): Boolean =
    fieldName.show === s || fieldAbbreviation.show === s
}

class FieldAbbreviation private(val abbr: Char) {
  override def toString = FieldAbbreviation.showInstance.show(this)
}

object FieldAbbreviation {
  private val fieldAbbreviationCanditateRegex = "^[a-zA-Z]$"
  private val fieldAbbreviationRegex = "^-[a-zA-Z]$"
  private val multiFieldAbbreviationRegex = "^-[a-zA-Z]+"

  def isFieldAbbreviation(value: String): Boolean = value.matches(fieldAbbreviationRegex)
  def splitMultiFieldAbbreviation(value: String): Seq[String] =
    if (value.matches(multiFieldAbbreviationRegex)) {
      value.tail.map(v => s"-$v")
    } else Seq(value)

  def apply(abbr: Char): FieldAbbreviation = {
    if (!abbr.toString.matches(fieldAbbreviationCanditateRegex))
      throw new IllegalArgumentException("Field abbreviation needs to be a letter")

    new FieldAbbreviation(abbr)
  }

  implicit def showInstance: Show[FieldAbbreviation] = new Show[FieldAbbreviation] {
    override def show(f: FieldAbbreviation): String = s"-${f.abbr}"
  }
}

class FieldName private(val name: String) {
  override def toString = FieldName.showInstance.show(this)
}

object FieldName {
  private val fieldNameCanditateRegex = "^[a-zA-Z]([a-zA-Z0-9]|[_-])*[a-zA-Z0-9]$"
  private val fieldNameRegex = "^--[a-zA-Z]([a-zA-Z0-9]|[_-])*[a-zA-Z0-9]$"

  def isFieldName(value: String): Boolean = value.matches(s"$fieldNameRegex")

  def apply(name: String): FieldName = {
    if (!name.toString.matches(fieldNameCanditateRegex))
      throw new IllegalArgumentException(
        "Field name needs to start with letters and only include letters, numbers, dashes or underscores")

    new FieldName(name)
  }

  implicit def showInstance: Show[FieldName] = new Show[FieldName] {
    override def show(f: FieldName): String = s"--${f.name}"
  }
}