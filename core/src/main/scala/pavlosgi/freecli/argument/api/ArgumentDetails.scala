package pavlosgi.freecli.argument.api

import cats.Show
import cats.syntax.show._

import pavlosgi.freecli.core._

case class ArgumentDetails(
  name: Option[ArgumentName],
  description: Option[Description]) {

  def shortDescription: String = {
    name.map(_.show).orElse(description.map(_.show)).getOrElse("")
  }

  def longDescription: String = {
    s"${showWithSpace(name)}${showWithSpace(description)}"
  }
}

object ArgumentDetails {
  implicit object showInstance extends Show[ArgumentDetails] {
    override def show(f: ArgumentDetails): String = {
      f.longDescription
    }
  }
}
