package pavlosgi.freecli.core.interpreters.parser.config

import cats.data.NonEmptyList

import pavlosgi.freecli.core.interpreters.parser.{arguments => A}
import pavlosgi.freecli.core.interpreters.parser.{options => O}

sealed trait ConfigParsingError {
  def message: String
}

case class ArgumentErrors(errors: NonEmptyList[A.ArgumentParsingError])
  extends ConfigParsingError {

  def message = errors.toList.map(_.message).mkString(", ")
}

case class OptionErrors(errors: NonEmptyList[O.OptionParsingError])
  extends ConfigParsingError {

  def message = errors.toList.map(_.message).mkString(", ")
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

  val message = s"Additional arguments found ${args.mkString(", ")}"
}
