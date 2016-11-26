package pavlosgi.freecli.core.api.config

import cats.free.FreeApplicative

import pavlosgi.freecli.core.api.Description

private[core] sealed trait Algebra[A]

private[core] final case class Arg[T, A](
  details: ArgumentDetails,
  f: T => A,
  g: StringDecoder[T])
  extends Algebra[A]

private[core] final case class RequiredOpt[T, A](
  field: Field,
  f: T => A,
  g: StringDecoder[T],
  default: Option[T])
  extends Algebra[A]

private[core] final case class Opt[T, A](
  field: Field,
  f: Option[T] => A,
  g: StringDecoder[T])
  extends Algebra[A]

private[core] final case class Flag[A](
  field: Field,
  f: Boolean => A)
  extends Algebra[A]

private[core] final case class Sub[A](
  description: Description,
  f: FreeApplicative[Algebra, A])
  extends Algebra[A]