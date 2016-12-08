package pavlosgi.freecli.argument.interpreters.help

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.argument.api.ArgumentDetails
import pavlosgi.freecli.core._

case class HelpState(arguments: List[ArgumentDetails])

object HelpState {
  implicit def monoidInstance = new Monoid[HelpState] {
    def combine(x: HelpState, y: HelpState): HelpState = {
      HelpState(x.arguments |+| y.arguments)
    }

    def empty = HelpState(List.empty)
  }

  def oneline(help: HelpState): String = {
    help.arguments.zipWithIndex.map {
      case (a, idx) => s"<${a.name.fold(s"arg${idx + 1}")(_.show)}>"
    }.mkString(" ")
  }

  def calculateMaxNameLength(a: HelpState): Int = {
    a.arguments.zipWithIndex.map {
      case (arg, idx) =>
        displayArgumentName(idx, arg).lengthExclANSI

    }.maximumOption.getOrElse(0)
  }

  def displayArgumentName(idx: Int, a: ArgumentDetails): String = {
    a.name.fold(s"arg${idx + 1}")(_.show).yellow
  }

  def display(
    indentation: Int,
    a: HelpState,
    currMaxNameLength: Option[Int] = None) = {

    val maxNameLength =
      HelpState.calculateMaxNameLength(a).max(currMaxNameLength.getOrElse(0))

    val res = a.arguments.zipWithIndex.map {
      case (a, idx) =>
        val pair =
          a.name.fold(s"arg${idx + 1}")(_.show).yellow ->
          a.description.fold("")(_.show)

        val space = maxNameLength - pair._1.lengthExclANSI + 2
        pair._1 + " " * space + pair._2

    }.mkString("\n")

    indent(indentation, res)
  }
}