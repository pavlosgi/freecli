package pavlosgi.freecli.core.api.arguments

import pavlosgi.freecli.core.api.StringDecoder

sealed trait Algebra[A]

final case class Arg[T, A](
  details: ArgumentDetails,
  f: T => A,
  g: StringDecoder[T])
  extends Algebra[A]