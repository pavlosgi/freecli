package pavlosgi.freecli.core.api.config

import cats.Show
import cats.syntax.show._

import pavlosgi.freecli.core.api.Description

private[core] case class ArgumentDetails(
  name: Option[ArgumentName],
  description: Option[Description])

private[core] object ArgumentDetails {
  implicit object showInstance extends Show[ArgumentDetails] {
    override def show(f: ArgumentDetails): String = {
      f match {
        case ArgumentDetails(None, None) => ""
        case ArgumentDetails(Some(p), None) => p.show
        case ArgumentDetails(None, Some(d)) => d.show
        case ArgumentDetails(Some(p), Some(d)) => s"$p $d"
      }
    }
  }
}
