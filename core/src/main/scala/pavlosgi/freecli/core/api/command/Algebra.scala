package pavlosgi.freecli.core.api.command

import cats.Alternative

import pavlosgi.freecli.core.api.AlgebraDependency
import pavlosgi.freecli.core.api.config.{Algebra => ConfigAlgebra}

abstract class Algebra[F[_]] extends Alternative[F] {

  def partialCmd[P](
    field: CommandField,
    run: P => Unit):
    F[PartialCommand[P]]

  def partialCmdWithConfig[H[_], C[_], A, P](
    field: CommandField,
    config: H[A],
    run: A => P => Unit)
   (implicit ev: AlgebraDependency[ConfigAlgebra, F, C],
    ev2: H[A] => C[A]):
    F[PartialCommand[P]]

  def partialParentCmd[P, G[_]](
    field: CommandField,
    subs: G[PartialCommand[P]])
   (implicit ev: G[PartialCommand[P]] => F[PartialCommand[P]]):
    F[PartialCommand[P]]

  def partialParentCmdWithConfig[H[_], C[_], A, P, G[_]](
    field: CommandField,
    config: H[A],
    subs: G[A => PartialCommand[P]])
   (implicit ev: AlgebraDependency[ConfigAlgebra, F, C],
    ev2: H[A] => C[A],
    ev4: G[A => PartialCommand[P]] => F[A => PartialCommand[P]]):
    F[PartialCommand[P]]
}



