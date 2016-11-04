package pavlosgi
package freecli
package core
package command
package api

import cats.Show
import cats.instances.all._
import cats.syntax.all._

case class CommandField(name: CommandFieldName, description: Option[Description]) {
  def matches(s: String): Boolean = name.show === s
}

class CommandFieldName private(val name: String) {
  override def toString = CommandFieldName.showInstance.show(this)
}

object CommandFieldName {
  private val commandFieldNameCanditateRegex = "^[a-zA-Z]([a-zA-Z0-9]|[_-])*[a-zA-Z0-9]$"
  private val commandFieldNameRegex = "^--[a-zA-Z]([a-zA-Z0-9]|[_-])*[a-zA-Z0-9]$"

  def isCommandFieldName(value: String): Boolean =
    value.matches(s"$commandFieldNameRegex")

  def apply(name: String): CommandFieldName = {
    if (!name.toString.matches(commandFieldNameCanditateRegex))
      throw new IllegalArgumentException(
        "Field name needs to start with letters and only include letters, numbers, dashes or underscores")

    new CommandFieldName(name)
  }

  implicit def showInstance: Show[CommandFieldName] = new Show[CommandFieldName] {
    override def show(f: CommandFieldName): String = s"${f.name}"
  }
}