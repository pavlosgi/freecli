package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.config.Description

trait FieldDescriptionDslOps[H <: HList] { self: Builder[H] =>
  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    append(description)
}
