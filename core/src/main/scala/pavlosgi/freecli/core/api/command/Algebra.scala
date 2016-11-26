package pavlosgi.freecli.core.api.command

import cats.free.FreeApplicative

import pavlosgi.freecli.core.api.config.{Algebra => ConfigAlgebra}
import pavlosgi.freecli.core.free.FreeAlternative

sealed abstract class Algebra[A]

case class PartialCmd[P, A](
  field: CommandField,
  run: P => Unit,
  f: PartialCommand[P] => A)
  extends Algebra[A]

case class PartialCmdWithConfig[C, P, A](
  field: CommandField,
  config: FreeApplicative[ConfigAlgebra, C],
  run: C => P => Unit,
  f: PartialCommand[P] => A)
  extends Algebra[A]

case class PartialParentCmd[P, A](
  field: CommandField,
  subs: FreeAlternative[Algebra, PartialCommand[P]],
  f: PartialCommand[P] => A)
  extends Algebra[A]

case class PartialParentCmdWithConfig[C, P, A](
  field: CommandField,
  config: FreeApplicative[ConfigAlgebra, C],
  subs: FreeAlternative[Algebra, C => PartialCommand[P]],
  f: PartialCommand[P] => A)
  extends Algebra[A]



