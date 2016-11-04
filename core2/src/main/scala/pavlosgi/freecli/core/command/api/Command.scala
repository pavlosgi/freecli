package pavlosgi.freecli.core.command.api

sealed trait Command {
  def run: Unit
}