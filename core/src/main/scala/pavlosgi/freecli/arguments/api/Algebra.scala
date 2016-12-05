package pavlosgi.freecli.arguments.api

import pavlosgi.freecli.core.StringDecoder

sealed trait Algebra[A]

final case class Arg[T, A](
  details: ArgumentDetails,
  f: T => A,
  g: StringDecoder[T])
  extends Algebra[A]