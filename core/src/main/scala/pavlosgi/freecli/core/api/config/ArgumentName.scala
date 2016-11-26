package pavlosgi.freecli.core.api.config

import cats.Show

private[core] case class ArgumentName(value: String)
private[core] object ArgumentName {
  implicit object showInstance extends Show[ArgumentName] {
    override def show(f: ArgumentName): String = f.value
  }
}