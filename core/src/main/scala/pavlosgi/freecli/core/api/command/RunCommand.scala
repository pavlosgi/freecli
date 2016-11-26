package pavlosgi.freecli.core.api.command

private[core] case class RunCommand[T](f: T => Unit)
