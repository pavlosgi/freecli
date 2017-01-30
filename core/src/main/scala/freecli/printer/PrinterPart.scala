package freecli
package printer

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