package pavlosgi.freecli.option.interpreters.help

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core._
import pavlosgi.freecli.option.api._
import pavlosgi.freecli.printer.{Printer, PrinterParts}
import pavlosgi.freecli.printer._

sealed trait OptionHelp
case class SingleOptionHelp(field: Field, default: Option[String], required: Boolean)
  extends OptionHelp

case class SubOptionHelp(description: String, options: OptionsHelp)
  extends OptionHelp

case class OptionsHelp(list: List[OptionHelp]) {

  def result: PrinterParts = {
    list.traverseU {
      case SingleOptionHelp(field, default, required) =>
        for {
          _ <- Printer.row
          _ <- Printer.col(field.shortDescription.yellow)
          modifiers = default.fold(if (required) "required".bold else "")(_.bold)
          _ <- Printer.col(modifiers)
          _ <- Printer.optCol(field.description.map(_.value))
          _ <- Printer.endRow
        } yield ()

      case SubOptionHelp(description, options) =>
        for {
          _ <- Printer.row
          _ <- Printer.col(description)
          _ <- Printer.endRow
          _ <- Printer.addFlat(options.result)
        } yield ()

    }.run
  }
}

object OptionsHelp {
  def single(o: OptionHelp) = OptionsHelp(List(o))

  implicit object monoidInstance extends Monoid[OptionsHelp] {
    def empty: OptionsHelp = OptionsHelp(List.empty)
    def combine(x: OptionsHelp, y: OptionsHelp): OptionsHelp = {
      OptionsHelp(x.list ++ y.list)
    }
  }
}
