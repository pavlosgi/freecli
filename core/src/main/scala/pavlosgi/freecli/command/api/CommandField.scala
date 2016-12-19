package pavlosgi.freecli.command.api

import cats.Show
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core.Description
import pavlosgi.freecli.core.formatting._

case class CommandField(name: CommandFieldName, description: Option[Description]) {
  def matches(s: String): Boolean = name.show === s
  def shortDescription: String = name.show
  def longDescription: String = s"${name.show}${showWithSpace(description)}"
}

object CommandField {
  implicit object showInstance extends Show[CommandField] {
    override def show(f: CommandField): String =
      f.name.show + f.description.fold("")(d => " " + d)
  }
}

class CommandFieldName private(val name: String) {
  override def toString = CommandFieldName.showInstance.show(this)

  def canEqual(other: Any): Boolean = other.isInstanceOf[CommandFieldName]

  override def equals(other: Any): Boolean = other match {
    case that: CommandFieldName =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object CommandFieldName {
  private val commandFieldNameCanditateRegex = "^[a-zA-Z]([a-zA-Z0-9]|[_-])*[a-zA-Z0-9]$"
  private val commandFieldNameRegex = "^[a-zA-Z]([a-zA-Z0-9]|[_-])*[a-zA-Z0-9]$"

  def isCommandFieldName(value: String): Boolean =
    value.matches(s"$commandFieldNameRegex")

  def apply(name: String): CommandFieldName = {
    if (!name.toString.matches(commandFieldNameCanditateRegex))
      throw new IllegalArgumentException(
        "Command Field name needs to start with letters and only include letters, numbers, dashes or underscores")

    new CommandFieldName(name)
  }

  implicit def showInstance: Show[CommandFieldName] =
    (f: CommandFieldName) => s"${f.name}"
}