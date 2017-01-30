package freecli
package argument
package help

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import api._
import core.formatting._
import printer.{Printer, PrinterParts}

case class ArgumentsHelp(list: List[ArgumentField]) {
  def oneline: PrinterParts = {
    list.zipWithIndex.traverseU {
      case (ArgumentField(Some(name), _), _) =>
        Printer.appendWithSpace(s"<${name.value}>")

      case (ArgumentField(None, _), idx) =>
        Printer.appendWithSpace(s"<arg${idx + 1}>")

    }.run
  }

  def result: PrinterParts = {
    list.zipWithIndex.traverseU {
      case (ArgumentField(Some(name), description), _) =>
        for {
          _ <- Printer.row
          _ <- Printer.col(name.value.yellow)
          _ <- Printer.col("")
          _ <- Printer.optCol(description.map(_.value))
          _ <- Printer.endRow
        } yield ()

      case (ArgumentField(None, description), idx) =>
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

