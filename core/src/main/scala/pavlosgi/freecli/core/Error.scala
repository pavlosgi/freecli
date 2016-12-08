package pavlosgi.freecli.core

import cats.data.NonEmptyList

trait Error[E] {
  def message(error: E): String
}

object Error {
  def displayErrors[E](e: NonEmptyList[E])(implicit ev: Error[E]): String = {
    s"""
   |${"Errors:".underline.bold.red}
   |
   |${indent(2, e.toList.map(ev.message).mkString("\n"))}""".stripMargin
  }
}
