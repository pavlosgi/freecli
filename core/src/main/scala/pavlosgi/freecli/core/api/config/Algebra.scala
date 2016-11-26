package pavlosgi.freecli.core.api.config

import cats.free.FreeApplicative

import pavlosgi.freecli.core.api.Description

trait Algebra[A]

case class Arg[T, A](
  details: ArgumentDetails,
  f: T => A,
  g: StringDecoder[T])
  extends Algebra[A]

case class RequiredOpt[T, A](
  field: Field,
  f: T => A,
  g: StringDecoder[T],
  default: Option[T])
  extends Algebra[A]

case class Opt[T, A](
  field: Field,
  f: Option[T] => A,
  g: StringDecoder[T])
  extends Algebra[A]

case class Flag[A](
  field: Field,
  f: Boolean => A)
  extends Algebra[A]

case class Sub[A](
  description: Description,
  f: FreeApplicative[Algebra, A])
  extends Algebra[A]