package pavlosgi.freecli.core.api.options

import cats.free.FreeApplicative

import pavlosgi.freecli.core.api.{Description, StringDecoder}

sealed trait Algebra[A]

final case class RequiredOpt[T, A](
  field: Field,
  f: T => A,
  g: StringDecoder[T],
  default: Option[T])
  extends Algebra[A]

final case class Opt[T, A](
  field: Field,
  f: Option[T] => A,
  g: StringDecoder[T])
  extends Algebra[A]

final case class Flag[A](
  field: Field,
  f: Boolean => A)
  extends Algebra[A]

final case class Sub[A](
  description: Description,
  f: FreeApplicative[Algebra, A])
  extends Algebra[A]