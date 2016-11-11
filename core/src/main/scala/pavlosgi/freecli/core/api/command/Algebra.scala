package pavlosgi.freecli.core.api.command

import cats.{Alternative, ~>}

import pavlosgi.freecli.core.api.options.{Algebra => OptionsAlgebra}

abstract class Algebra[F[_], C[_]: OptionsAlgebra]
  extends Alternative[F] {

  implicit def configNat: C ~> F

  def partialCmd[P](
    field: CommandField,
    run: P => Unit):
    F[PartialCommand[P]]

  def partialCmdWithConfig[H[_], A, P](
    field: CommandField,
    config: H[A],
    run: A => P => Unit)
   (implicit ev: H[A] => C[A]):
    F[PartialCommand[P]]

  def partialParentCmd[P, G[_]](
    field: CommandField,
    subs: G[PartialCommand[P]])
   (implicit ev: G[PartialCommand[P]] => F[PartialCommand[P]]):
    F[PartialCommand[P]]

  def partialParentCmdWithConfig[H[_], A, P, G[_]](
    field: CommandField,
    config: H[A],
    subs: G[A => PartialCommand[P]])
   (implicit ev: H[A] => C[A],
    ev2: G[A => PartialCommand[P]] => F[A => PartialCommand[P]]):
    F[PartialCommand[P]]
}



