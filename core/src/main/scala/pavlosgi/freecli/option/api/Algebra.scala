package pavlosgi.freecli.option.api

import cats.free.FreeApplicative

import pavlosgi.freecli.core.{Description, StringDecoder}

sealed trait Algebra[A]

final case class RequiredOpt[T, A](
  field: OptionField,
  f: T => A,
  g: StringDecoder[T],
  default: Option[T])
  extends Algebra[A]

final case class Opt[T, A](
  field: OptionField,
  f: Option[T] => A,
  g: StringDecoder[T])
  extends Algebra[A]

final case class Flag[A](
  field: OptionField,
  f: Boolean => A)
  extends Algebra[A]

final case class Sub[A](
  description: Description,
  f: FreeApplicative[Algebra, A])
  extends Algebra[A]