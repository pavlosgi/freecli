package freecli
package config
package parser

import cats.data.NonEmptyList

import argument.{parser => A}
import option.{parser => O}
import freecli.parser.DisplayErrors

sealed trait ConfigParsingError {
  def message: String
}

object ConfigParsingError {
  implicit object displayErrorsInstance extends DisplayErrors[NonEmptyList[ConfigParsingError]] {
    override def display(errors: NonEmptyList[ConfigParsingError]): String = {
      errors.map(_.message).toList.mkString("\n")
    }
  }
}

case class ArgumentErrors(errors: NonEmptyList[A.ArgumentParsingError])
  extends ConfigParsingError {

  def message = errors.toList.map(_.message).mkString("\n")
}

case class OptionErrors(errors: NonEmptyList[O.OptionParsingError])
  extends ConfigParsingError {

  def message = errors.toList.map(_.message).mkString("\n")
}

case class OptionAndArgumentErrors(
  optionErrors: NonEmptyList[A.ArgumentParsingError],
  argumentErrors: NonEmptyList[O.OptionParsingError])
  extends ConfigParsingError {

  def message =
    s"""Option errors, ${optionErrors.toList.map(_.message).mkString(", ")}
       |Argument errors, ${argumentErrors.toList.map(_.message).mkString(", ")}
     """.stripMargin
}

case class AdditionalArgumentsFound(args: Seq[String])
  extends ConfigParsingError  {

  def message = s"Additional arguments found: ${args.mkString(", ")}"
}
