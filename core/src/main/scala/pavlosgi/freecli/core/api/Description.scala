package pavlosgi.freecli.core.api

import cats.Show

private[core] case class Description(value: String)

private[core] object Description {
  implicit object showInstance extends Show[Description] {
    override def show(f: Description): String = s"${f.value}"
  }
}