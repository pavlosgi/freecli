package pavlosgi.freecli.argument.api

import cats.Show
import cats.syntax.show._

import pavlosgi.freecli.core.Description
import pavlosgi.freecli.core.formatting._

case class ArgumentField(
  name: Option[ArgumentFieldName],
  description: Option[Description]) {

  def shortDescription: String = {
    name.map(_.show).orElse(description.map(_.show)).getOrElse("")
  }

  def longDescription: String = {
    s"${showWithSpace(name)}${showWithSpace(description)}"
  }
}

object ArgumentField {
  implicit object showInstance extends Show[ArgumentField] {
    override def show(f: ArgumentField): String = {
      f.longDescription
    }
  }
}
