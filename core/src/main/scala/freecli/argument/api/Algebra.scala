package freecli
package argument
package api

import core.api.StringDecoder

sealed trait Algebra[A]

final case class Arg[T, A](
  details: ArgumentField,
  f: T => A,
  g: StringDecoder[T])
  extends Algebra[A]