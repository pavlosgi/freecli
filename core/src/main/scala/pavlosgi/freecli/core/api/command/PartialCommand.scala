package pavlosgi.freecli.core.api.command

private[core] case class PartialCommand[P](f: P => Command)
