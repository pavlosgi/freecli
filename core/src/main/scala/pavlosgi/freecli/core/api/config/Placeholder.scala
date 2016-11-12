package pavlosgi.freecli.core.api.config

import cats.Show

case class Placeholder(value: String)
object Placeholder {
  implicit object showInstance extends Show[Placeholder] {
    override def show(f: Placeholder): String = f.value
  }
}