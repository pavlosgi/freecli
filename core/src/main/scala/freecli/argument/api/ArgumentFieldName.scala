package freecli
package argument
package api

import cats.Show

case class ArgumentFieldName(value: String)

object ArgumentFieldName {
  implicit object showInstance extends Show[ArgumentFieldName] {
    override def show(f: ArgumentFieldName): String = f.value
  }
}