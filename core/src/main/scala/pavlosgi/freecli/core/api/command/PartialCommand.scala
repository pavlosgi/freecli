package pavlosgi.freecli.core.api.command

case class PartialCommand[P](f: P => Command)
