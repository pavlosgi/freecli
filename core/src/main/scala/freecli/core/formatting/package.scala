package freecli
package core

import cats.Show
import cats.syntax.show._

package object formatting {
  @inline
  implicit def str2FormattingOps(s: String): StringFormattingOps = {
    new StringFormattingOps(s)
  }

  def indent(indentation: Int, s: String): String = {
    val lines = s.split("\n")
    val i = (0 until indentation).foldLeft("")((a, _) => a + " ")

    lines.map(l => s"$i$l").mkString("\n")
  }

  def contentWithTitle(title: String, s: String) = {
    s"""$title
       |$s""".stripMargin
  }

  def optionalContentWithTitle(title: String, s: Option[String]) = {
    s match {
      case None => None
      case Some(v) => Some(contentWithTitle(title, v))
    }
  }

  def optionalPair(v1: Option[String], v2: Option[String], spaceDelimiter: String): String = {
    v1 -> v2 match {
      case (Some(v1_), Some(v2_)) => s"$v1_$spaceDelimiter$v2_"
      case (Some(v1_), None) => s"$v1_"
      case (None, Some(v2_)) => s"$v2_"
      case (None, None) => s""
    }
  }

  def showWithSpace[S: Show](o: Option[S]) = {
    o.fold("")(v => s" ${v.show}")
  }
}
