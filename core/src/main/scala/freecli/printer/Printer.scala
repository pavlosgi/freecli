package freecli
package printer

import cats.data.State
import cats.Monad

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
