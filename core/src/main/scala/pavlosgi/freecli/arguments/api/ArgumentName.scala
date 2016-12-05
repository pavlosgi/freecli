package pavlosgi.freecli.arguments.api

import cats.Show

case class ArgumentName(value: String)

object ArgumentName {
  implicit object showInstance extends Show[ArgumentName] {
    override def show(f: ArgumentName): String = f.value
  }
}