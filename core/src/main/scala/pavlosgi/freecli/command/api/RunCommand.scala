package pavlosgi.freecli.command.api

case class RunCommand[T](f: T => Unit)
