package pavlosgi.freecli.config.interpreters.help

import cats.Monoid
import cats.instances.all._
import cats.syntax.all._

import pavlosgi.freecli.argument.interpreters.{help => A}
import pavlosgi.freecli.core._
import pavlosgi.freecli.option.interpreters.{help => O}

case class HelpState(options: Option[O.HelpState], arguments: Option[A.HelpState])

object HelpState {
  def display(indentation: Int, s: HelpState): String = {
    val optsMax = s.options.fold(0)(O.HelpState.calculateMaxNameLength)
    val argsMax = s.arguments.fold(0)(A.HelpState.calculateMaxNameLength)

    Seq(
      optionalContentWithTitle(
        indent(indentation, "Options"),
        s.options.map(v => O.HelpState.display(indentation + 2, v, Some(optsMax)))),

      optionalContentWithTitle(
        indent(indentation, "Arguments"),
        s.arguments.map(v => A.HelpState.display(indentation + 2, v, Some(argsMax))))

    ).flatten.mkString("\n\n")
  }

  implicit def monoidInstance: Monoid[HelpState] = new Monoid[HelpState] {
    def empty: HelpState = HelpState(None, None)
    def combine(x: HelpState, y: HelpState): HelpState = {
      HelpState(
        x.options |+| y.options,
        x.arguments |+| y.arguments)
    }
  }
}