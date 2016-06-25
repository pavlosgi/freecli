package pavlosgi
package freecli
package commands
package algebra

import pavlosgi.freecli.config.algebra.{ApplyConfigAlgebra, Plugin}

import cats.{Alternative, Show}

abstract class ApplyCommandAlgebra[G[_]: Plugin, A] {
  def apply[F[_]: CommandAlgebra[?[_], G]]: F[A]
}

abstract class CommandAlgebra[F[_], G[_]: Plugin] extends Alternative[F] {
  def cmd(
    field: CommandField,
    run: => Unit,
    f: ApplyCommandAlgebra[G, Command]
  ): F[Command]

  def cmdWithConfig[A](
    field: CommandField,
    config: ApplyConfigAlgebra[G, A],
    run: A => Unit,
    f: ApplyCommandAlgebra[G, Command]
  ): F[Command]

}

case class CommandField(name: CommandFieldName, description: Option[Description])
object CommandField {
  implicit object showInstance extends Show[CommandField] {
    override def show(f: CommandField): String = {
      val descr = f.description.map { d =>
                    s" ${Description.showInstance.show(d)}"
                  }.getOrElse("")

      s"${CommandFieldName.showInstance.show(f.name)}$descr"
    }
  }
}

class CommandFieldName private(val name: String) {
  override def toString = CommandFieldName.showInstance.show(this)
}

object CommandFieldName {
  def apply(name: String): CommandFieldName = {
    val fname = name.replaceAll(" ", "-").replaceAll("([a-z])([A-Z])", "$1-$2").toLowerCase
    new CommandFieldName(fname)
  }

  implicit object showInstance extends Show[CommandFieldName] {
    override def show(f: CommandFieldName): String = s"${f.name}"
  }
}

case class Description(value: String)
object Description {
  implicit object showInstance extends Show[Description] {
    override def show(f: Description): String = s"${f.value}"
  }
}

trait Command {
  def run: Unit
}