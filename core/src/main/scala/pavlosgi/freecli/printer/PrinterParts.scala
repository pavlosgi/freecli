package pavlosgi.freecli.printer

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core.formatting._

case class PrinterParts(reverseList: List[PrinterPart]) {
  def display(indentation: Int = 0, colSpacing: Int = 2, maxColLength: Int = 30): String = {
    val list = reverseList.reverse

    case class LongestColumns(map: Map[Int, Int], idx: Int)
    val longestColumns = list.foldLeft(Map.empty[Int, Int]) {
      case (curr, Row(cols)) =>
        curr ++ cols.zipWithIndex.map {
          case (s, idx) =>
            val max1 = Math.max(s.lengthExclANSI, curr.getOrElse(idx, 0))
            val max2 = if (max1 > maxColLength) maxColLength else max1
            val max3 = if (max2 % 2 =!= 0) max2 + 1 else max2
            idx -> max3

        }.toMap

      case (curr, _) => curr
    }

    case class LocalState(indentation: Int, str: String)
    list.zipWithIndex.foldLeft(LocalState(indentation, "")) {
      case (state, (Empty, _)) => state
      case (state, (EndRow, _)) => state
      case (LocalState(i, s), (Row(cols), _)) =>
        val colStrings = cols.zipWithIndex.map {
          case (c, index) =>
          val colSpace = longestColumns.getOrElse(index, 0) - c.lengthExclANSI
          s"$c${" " * colSpace}"
        }

        val newline = if (s.isEmpty || s.endsWith("\n")) "" else "\n"
        val ind = " " * i
        LocalState(
          indentation,
          s"$s$newline$ind${colStrings.mkString(" " * colSpacing)}\n")

      case (LocalState(i, s), (Line(v), _)) =>
        val newline = if (s.isEmpty || s.endsWith("\n")) "" else "\n"
        LocalState(i, s"$s$newline${" " * i}$v\n")

      case (LocalState(i, s), (Indent(v), _)) =>
        LocalState(i + v, s)

      case (LocalState(i, s), (NewLine, _)) =>
        LocalState(i, s"$s\n")

      case (LocalState(i, s), (EnsureSingleLineSpace, idx)) =>
        if (idx > 0 && idx < list.size - 1) {
          s.drop(s.length - 2) match {
            case lastTwo if lastTwo === "\n\n" => LocalState(i, s)
            case lastTwo if lastTwo.drop(1) === "\n" => LocalState(i, s"$s\n")
            case _ => LocalState(i, s"$s\n\n")
          }
        } else {
          LocalState(i, s)
        }

      case (LocalState(i, s), (AppendRow(v), _)) =>
        LocalState(i, s"$s${" " * i}${v.mkString}")

      case (LocalState(i, s), (SingleSpacedRow(c), _)) =>
        LocalState(i, s"$s${" " * i}${c.mkString(" ")}")

      case (LocalState(i, s), (Add(p), _)) =>
        LocalState(i, s"$s${p.display(i)}")

    }.str
  }
}

object PrinterParts {
  implicit object monoidInstance extends Monoid[PrinterParts] {
    def empty: PrinterParts = PrinterParts(List.empty)
    def combine(x: PrinterParts, y: PrinterParts): PrinterParts = {
      PrinterParts(y.reverseList ++ x.reverseList)
    }
  }

  def empty = Monoid[PrinterParts].empty
}

