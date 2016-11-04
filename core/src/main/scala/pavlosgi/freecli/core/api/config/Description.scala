package pavlosgi.freecli.core.api.config

import cats.Show

case class Description(value: String)

object Description {
  implicit object showInstance extends Show[Description] {
    override def show(f: Description): String = s"${f.value}"
  }
}