package pavlosgi.freecli.printer

import cats.data.State
import cats.{Monad, Monoid}
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.core._

sealed trait PrinterPart
case object Empty extends PrinterPart
case class Row(cols: List[String]) extends PrinterPart
case object EndRow extends PrinterPart
case class Indent(i: Int) extends PrinterPart
case class Line(str: String) extends PrinterPart
case object NewLine extends PrinterPart
case object EnsureSingleLineSpace extends PrinterPart
case class AppendRow(a: List[String]) extends PrinterPart
case class SingleSpacedRow(cols: List[String]) extends PrinterPart
case class Add(p: PrinterParts) extends PrinterPart

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

case class Printer[A](state: State[PrinterParts, A]) {
  def run: PrinterParts = state.runS(PrinterParts(List.empty)).value
}

object Printer {
  implicit object monadInstance extends Monad[Printer] {
    def flatMap[A, B](fa: Printer[A])(f: A => Printer[B]): Printer[B] = {
      Printer(fa.state.flatMap(a => f(a).state))
    }

    def tailRecM[A, B](a: A)(f: A => Printer[Either[A, B]]): Printer[B] = {
      Printer(implicitly[Monad[State[PrinterParts, ?]]]
        .tailRecM(a)(a => f(a).state))
    }

    def pure[A](x: A): Printer[A] = Printer(State.pure(x))
  }

  private def modify(x: PrinterPart): Printer[Unit] = {
    Printer(State.modify(p => p.copy(reverseList = x +: p.reverseList)))
  }

  def empty = modify(Empty)
  def row = modify(Row(List.empty))
  def endRow = modify(EndRow)
  def col(str: String) = {
    Printer(State.modify { p =>
      p.reverseList match {
        case Row(c) :: t => PrinterParts(Row(c :+ str) +: t)
        case xs => PrinterParts(Row(List(str)) +: xs)
      }
    })
  }

  def optCol(str: Option[String]) =
    str.fold(Monad[Printer].pure(()))(v => col(v))

  def indent(i: Int) = modify(Indent(i))
  def line(str: String) = modify(Line(str))
  def newLine = modify(NewLine)
  def ensureSingleLineSpace = modify(EnsureSingleLineSpace)
  def append(str: String) = {
    Printer(State.modify { p =>
      p.reverseList match {
        case AppendRow(c) :: t =>
          PrinterParts(AppendRow(c :+ str) +: t)

        case xs => PrinterParts(AppendRow(List(str)) +: xs)
      }
    })
  }

  def appendWithSpace(str: String) = {
    Printer(State.modify { p =>
      p.reverseList match {
        case SingleSpacedRow(c) :: t =>
          PrinterParts(SingleSpacedRow(c :+ str) +: t)

        case xs => PrinterParts(SingleSpacedRow(List(str)) +: xs)
      }
    })
  }

  def addFlat(p: PrinterParts) =
    Printer(State.modify(v => v.copy(reverseList = p.reverseList ++ v.reverseList)))

  def add(p: PrinterParts) = modify(Add(p))
}
