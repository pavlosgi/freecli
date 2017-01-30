package freecli
package option
package api

import cats.Show
import cats.instances.all._
import cats.syntax.all._

import core.api.Description
import core.formatting._

sealed trait OptionField {
  def description: Option[Description]
  def shortDescription: String
  def fullDescription: String
  def matches(s: String): Boolean
}

object OptionField {
  implicit def showInstance: Show[OptionField] = new Show[OptionField] {
    def show(f: OptionField): String = f match {
      case OptionFieldNameOnly(name, description) =>
        name.show + description.fold("")(d => s" ${d.show}")

      case OptionFieldAbbreviationOnly(abbr, description) =>
        abbr.show + description.fold("")(d => s" ${d.show}")

      case OptionFieldNameAndAbbreviation(name, abbr, description) =>
        name.show + ", " + abbr.show +
          description.fold("")(d => s" ${d.show}")
    }
  }

  def withFieldName(field: OptionField, name: OptionFieldName): OptionField = {
    field match {
      case OptionFieldNameOnly(_, des) => OptionFieldNameOnly(name, des)
      case OptionFieldAbbreviationOnly(abbr, des) =>
        OptionFieldNameAndAbbreviation(name, abbr, des)

      case OptionFieldNameAndAbbreviation(_, abbr, des) =>
        OptionFieldNameAndAbbreviation(name, abbr, des)
    }
  }

  def withFieldAbbreviation(field: OptionField, abbr: OptionFieldAbbreviation): OptionField = {
    field match {
      case OptionFieldNameOnly(name, des) => OptionFieldNameAndAbbreviation(name, abbr, des)
      case OptionFieldAbbreviationOnly(_, des) =>
        OptionFieldAbbreviationOnly(abbr, des)

      case OptionFieldNameAndAbbreviation(name, _, des) =>
        OptionFieldNameAndAbbreviation(name, abbr, des)
    }
  }

  def withDescription(field: OptionField, description: Description): OptionField = {
    field match {
      case OptionFieldNameOnly(name, _) => OptionFieldNameOnly(name, Some(description))
      case OptionFieldAbbreviationOnly(abbr, _) =>
        OptionFieldAbbreviationOnly(abbr, Some(description))

      case OptionFieldNameAndAbbreviation(name, abbr, _) =>
        OptionFieldNameAndAbbreviation(name, abbr, Some(description))
    }
  }

  def withOptionalDescription(field: OptionField, description: Option[Description]): OptionField = {
    field match {
      case OptionFieldNameOnly(name, _) => OptionFieldNameOnly(name, description)
      case OptionFieldAbbreviationOnly(abbr, _) =>
        OptionFieldAbbreviationOnly(abbr, description)

      case OptionFieldNameAndAbbreviation(name, abbr, _) =>
        OptionFieldNameAndAbbreviation(name, abbr, description)
    }
  }
}

case class OptionFieldNameOnly(
  fieldName: OptionFieldName,
  description: Option[Description]
) extends OptionField {
  def matches(s: String): Boolean = fieldName.show === s

  def shortDescription: String = fieldName.show
  def fullDescription: String = s"${fieldName.show}${showWithSpace(description)}"
}

case class OptionFieldAbbreviationOnly(
  fieldAbbreviation: OptionFieldAbbreviation,
  description: Option[Description]
) extends OptionField {
  def matches(s: String): Boolean = fieldAbbreviation.show === s
  def shortDescription: String = fieldAbbreviation.show
  def fullDescription: String = s"${fieldAbbreviation.show}${showWithSpace(description)}"
}

case class OptionFieldNameAndAbbreviation(
  fieldName: OptionFieldName,
  fieldAbbreviation: OptionFieldAbbreviation,
  description: Option[Description]
) extends OptionField {
  def matches(s: String): Boolean =
    fieldName.show === s || fieldAbbreviation.show === s

  def shortDescription: String = s"${fieldAbbreviation.show}, ${fieldName.show}"
  def fullDescription: String =
    s"${fieldName.show}, ${fieldAbbreviation.show}${showWithSpace(description)}"
}

class OptionFieldAbbreviation private(val abbr: Char) {
  override def toString = OptionFieldAbbreviation.showInstance.show(this)
}

object OptionFieldAbbreviation {
  private val fieldAbbreviationCandidateRegex = "^[a-zA-Z]$"
  private val fieldAbbreviationRegex = "^-[a-zA-Z]$"

  def isFieldAbbreviation(value: String): Boolean = value.matches(fieldAbbreviationRegex)

  def apply(abbr: Char): OptionFieldAbbreviation = {
    if (!abbr.toString.matches(fieldAbbreviationCandidateRegex))
      throw new IllegalArgumentException("Field abbreviation needs to be a letter")

    new OptionFieldAbbreviation(abbr)
  }

  implicit def showInstance: Show[OptionFieldAbbreviation] = new Show[OptionFieldAbbreviation] {
    def show(f: OptionFieldAbbreviation): String = s"-${f.abbr}"
  }
}

class OptionFieldName private(val name: String) {
  override def toString = OptionFieldName.showInstance.show(this)
}

object OptionFieldName {
  private val fieldNameCanditateRegex = "^[a-zA-Z]([a-zA-Z0-9]|[_-])*[a-zA-Z0-9]$"
  private val fieldNameRegex = "^--[a-zA-Z]([a-zA-Z0-9]|[_-])*[a-zA-Z0-9]$"

  def isFieldName(value: String): Boolean = value.matches(s"$fieldNameRegex")

  def apply(name: String): OptionFieldName = {
    if (!name.toString.matches(fieldNameCanditateRegex))
      throw new IllegalArgumentException(
        "Field name needs to start with letters and only include letters, numbers, dashes or underscores")

    new OptionFieldName(name)
  }

  implicit def showInstance: Show[OptionFieldName] = new Show[OptionFieldName] {
    def show(f: OptionFieldName): String = s"--${f.name}"
  }
}