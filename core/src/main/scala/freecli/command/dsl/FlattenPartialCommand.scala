package freecli
package command
package dsl

import shapeless._

import api.{Command, PartialCommand}

trait FlattenPartialCommand[P] {
  def apply(p: PartialCommand[P]): Command
}

object FlattenPartialCommand {
  implicit def flattenCommandHNil = new FlattenPartialCommand[HNil] {
    def apply(p: PartialCommand[HNil]): Command = p.f(HNil)
  }
}
