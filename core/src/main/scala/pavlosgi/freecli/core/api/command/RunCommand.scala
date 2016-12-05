package pavlosgi.freecli.core.api.command

case class RunCommand[T](f: T => Unit)
