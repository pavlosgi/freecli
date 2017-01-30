package freecli
package command
package api

import cats.free.FreeApplicative

import config.api.{Algebra => ConfigAlgebra}
import core.free.FreeAlternative

sealed trait Algebra[A]

final case class PartialCmd[P, A](
  field: CommandField,
  run: P => Unit,
  f: PartialCommand[P] => A)
  extends Algebra[A]

final case class PartialCmdWithConfig[C, P, A](
  field: CommandField,
  config: FreeApplicative[ConfigAlgebra, C],
  run: C => P => Unit,
  f: PartialCommand[P] => A)
  extends Algebra[A]

final case class PartialParentCmd[P, A](
  field: CommandField,
  subs: FreeAlternative[Algebra, PartialCommand[P]],
  f: PartialCommand[P] => A)
  extends Algebra[A]

final case class PartialParentCmdWithConfig[C, P, A](
  field: CommandField,
  config: FreeApplicative[ConfigAlgebra, C],
  subs: FreeAlternative[Algebra, C => PartialCommand[P]],
  f: PartialCommand[P] => A)
  extends Algebra[A]



