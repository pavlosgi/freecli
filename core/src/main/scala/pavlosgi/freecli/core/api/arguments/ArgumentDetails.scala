package pavlosgi.freecli.core.api.arguments

import cats.Show

case class ArgumentDetails(placeholder: Placeholder, description: Option[Description])
object ArgumentDetails {
  implicit object showInstance extends Show[ArgumentDetails] {
    override def show(f: ArgumentDetails): String =
      s"${f.placeholder} ${f.description.fold("")(_)}"
  }
}
