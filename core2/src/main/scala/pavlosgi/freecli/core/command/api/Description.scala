package pavlosgi
package freecli
package core
package command
package api

import cats.Show

case class Description(value: String)

object Description {
  implicit object showInstance extends Show[Description] {
    override def show(f: Description): String = s"${f.value}"
  }
}