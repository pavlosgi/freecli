package pavlosgi.freecli.config.api

import cats.free.FreeApplicative

import pavlosgi.freecli.argument.{api => A}
import pavlosgi.freecli.option.{api => O}

sealed trait Algebra[A]

final case class Args[A](
  dsl: FreeApplicative[A.Algebra, A])
  extends Algebra[A]

final case class Opts[A](
  dsl: FreeApplicative[O.Algebra, A])
  extends Algebra[A]

final case class OptsAndArgs[A, B, C](
  opts: FreeApplicative[O.Algebra, A],
  args: FreeApplicative[A.Algebra, B],
  f: (A, B) => C)
  extends Algebra[C]