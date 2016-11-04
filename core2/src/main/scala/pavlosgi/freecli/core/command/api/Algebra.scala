package pavlosgi.freecli.core.command.api

import cats.Alternative

import pavlosgi.freecli.core.api.config.{Algebra => ConfigAlgebra}

abstract class Algebra[F[_]: ConfigAlgebra] extends Alternative[F] {
  def configAlgebra = implicitly[ConfigAlgebra[F]]

  def cmd(
    name: CommandField,
    run: => Unit):
    F[Command]

  def cmdWithSubcommands[G[_]](
    name: CommandField,
    subcommands: G[Command])
   (implicit ev: G[Command] => F[Command]):
    F[Command]

  def cmdWithConfig[H[_], A](
    name: CommandField,
    config: H[A],
    run: A => Unit)
   (implicit ev: H[A] => F[A]):
    F[Command]

  def cmdWithConfigAndSubcommands[G[_], H[_], A](
    name: CommandField,
    config: H[A],
    subcommands: G[Command])
   (implicit ev: G[Command] => F[Command],
    ev2: H[A] => F[A]):
    F[Command]


//  def cmdWithConfig[G[_], A](
//    name: CommandField,
//    config: G[A],
//    run: A => Unit)
//   (implicit ev: G[A] => F[A]):
//    F[Command]
//
//  def cmdWithSubcommands[G[_]](
//    name: CommandField,
//    subcommands: G[Command])
//   (implicit ev: G[Command] => F[Command]):
//    F[Command]
//
//  def cmdWithConfigAndSubcommands[G[_], H[_], A](
//    name: CommandField,
//    config: G[A],
//    subcommands: A => H[Command])
//   (implicit ev: G[A] => F[A],
//    ev2: H[Command] => F[Command]):
//    F[Command]
}



