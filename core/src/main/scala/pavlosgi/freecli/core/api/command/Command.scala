package pavlosgi.freecli.core.api.command

import cats.syntax.show._

sealed trait Command {
  def field: CommandField
  def run(): Unit

  override def toString = field.show
}

private[core] object Command {
  def apply(commandField: CommandField, f: => Unit): Command = {
    new Command {
      def field: CommandField = commandField
      def run(): Unit = f
    }
  }
}



