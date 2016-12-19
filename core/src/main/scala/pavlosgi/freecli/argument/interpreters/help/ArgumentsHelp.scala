package pavlosgi.freecli.argument.interpreters.help

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.argument.api._
import pavlosgi.freecli.core.formatting._
import pavlosgi.freecli.printer.{Printer, PrinterParts}

case class ArgumentsHelp(list: List[ArgumentDetails]) {
  def oneline: PrinterParts = {
    list.zipWithIndex.traverseU {
      case (ArgumentDetails(Some(name), _), _) =>
        Printer.appendWithSpace(s"<${name.value}>")

      case (ArgumentDetails(None, _), idx) =>
        Printer.appendWithSpace(s"<arg${idx + 1}>")

    }.run
  }

  def result: PrinterParts = {
    list.zipWithIndex.traverseU {
      case (ArgumentDetails(Some(name), description), _) =>
        for {
          _ <- Printer.row
          _ <- Printer.col(name.value.yellow)
          _ <- Printer.col("")
          _ <- Printer.optCol(description.map(_.value))
          _ <- Printer.endRow
        } yield ()

      case (ArgumentDetails(None, description), idx) =>
        for {
          _ <- Printer.row
          _ <- Printer.col(s"arg${idx + 1}".yellow)
          _ <- Printer.col("")
          _ <- Printer.optCol(description.map(_.value))
          _ <- Printer.endRow
        } yield ()

    }.run
  }
}

object ArgumentsHelp {
  implicit object monoidInstance extends Monoid[ArgumentsHelp] {
    def empty: ArgumentsHelp = {
      ArgumentsHelp(List.empty)
    }

    def combine(x: ArgumentsHelp, y: ArgumentsHelp): ArgumentsHelp = {
      ArgumentsHelp(x.list ++ y.list)
    }
  }
}

