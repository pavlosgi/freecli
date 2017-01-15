package pavlosgi.freecli.command.dsl

import shapeless._

import pavlosgi.freecli.command.api.{Command, PartialCommand}

trait FlattenPartialCommand[P] {
  def apply(p: PartialCommand[P]): Command
}

object FlattenPartialCommand {
  implicit def flattenCommandHNil = new FlattenPartialCommand[HNil] {
    def apply(p: PartialCommand[HNil]): Command = p.f(HNil)
  }
}
