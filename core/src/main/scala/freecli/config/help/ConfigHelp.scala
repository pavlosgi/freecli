package freecli
package config
package help

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import argument.{help => A}
import option.{help => O}
import printer.{Printer, PrinterParts}

case class ConfigHelp(
  options: Option[O.OptionsHelp],
  arguments: Option[A.ArgumentsHelp]) {

  def oneline: PrinterParts =
    arguments.fold(PrinterParts.empty)(_.oneline)

  def result: PrinterParts = {
    val optionsResult =
      options.map { o =>
        Printer.add(o.result).run
      }

    val argumentsResult =
      arguments.map { a =>
        Printer.add(a.result).run
      }

    (optionsResult, argumentsResult) match {
      case (None, None) => Printer.empty.run
      case (Some(o), None) =>
        (for {
          _ <- Printer.line("Options")
          _ <- Printer.add(o)
        } yield ()).run

      case (None, Some(a)) =>
        (for {
          _ <- Printer.line("Arguments")
          _ <- Printer.add(a)
        } yield ()).run

      case (Some(o), Some(a)) =>
        (for {
          _ <- Printer.line("Options")
          _ <- Printer.add(o)
          _ <- Printer.newLine
          _ <- Printer.line("Arguments")
          _ <- Printer.add(a)
        } yield ()).run
    }
  }
}

object ConfigHelp {
  def options(o: O.OptionsHelp) = ConfigHelp(Some(o), None)
  def arguments(a: A.ArgumentsHelp) = ConfigHelp(None, Some(a))
  def optionsAndArguments(o: O.OptionsHelp, a: A.ArgumentsHelp) =
    ConfigHelp(Some(o), Some(a))

  implicit object monoidInstance extends Monoid[ConfigHelp] {
    def empty: ConfigHelp = ConfigHelp(None, None)
    def combine(x: ConfigHelp, y: ConfigHelp): ConfigHelp = {
      ConfigHelp(x.options |+| y.options, x.arguments |+| y.arguments)
    }
  }
}
