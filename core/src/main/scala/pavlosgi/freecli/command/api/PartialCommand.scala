package pavlosgi.freecli.command.api

case class PartialCommand[P](f: P => Command)
