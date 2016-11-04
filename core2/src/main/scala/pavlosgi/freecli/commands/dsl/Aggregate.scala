package pavlosgi
package freecli
package commands
package dsl

import shapeless.ops.hlist.LeftFolder
import shapeless.{::, HList, Poly2}

import algebra._
import config.algebra.Plugin
import dsl.config.ConfigDsl

sealed abstract class Aggregate[G[_]: Plugin, A]

object Aggregate {
  object aggregate extends Poly2 {
    implicit def caseAggregateNameAdded[G[_]: Plugin, A]:
      Case.Aux[Aggregate[G, A], NameAdded, Aggregate[G, A]] =

      at[Aggregate[G, A], NameAdded] {
        case (c: Aggregate[G, A], s: NameAdded) => c match {
          case CommandAggregate(_, d, r, subs) =>
            CommandAggregate(s.name, d, r, subs)

          case CommandWithConfigAggregate(_, d, conf, r, subs) =>
            CommandWithConfigAggregate(s.name, d, conf, r, subs)
        }
      }

    implicit def caseAggregateDescriptionAdded[G[_]: Plugin, A]:
      Case.Aux[Aggregate[G, A], DescriptionAdded, Aggregate[G, A]] =

      at[Aggregate[G, A], DescriptionAdded] {
        case (c: Aggregate[G, A], s: DescriptionAdded) => c match {
          case CommandAggregate(n, _, r, subs) =>
            CommandAggregate(n, Some(s.description), r, subs)

          case CommandWithConfigAggregate(n, _, conf, r, subs) =>
            CommandWithConfigAggregate(n, Some(s.description), conf, r, subs)
        }
      }

    implicit def caseAggregateCommandAdded[G[_]: Plugin, E <: HList, A, B]
      (implicit ev: LeftFolder.Aux[NameAdded :: E, Aggregate[G, Unit], aggregate.type, Aggregate[G, B]]):
      Case.Aux[Aggregate[G, A], CommandAdded[NameAdded :: E], Aggregate[G, A]] =

      at[Aggregate[G, A], CommandAdded[NameAdded :: E]] {
        case (c: Aggregate[G, A], s: CommandAdded[NameAdded :: E]) => c match {
          case CommandAggregate(n, d, r, subs) =>
            CommandAggregate(
              n, d, r, subs ++ Seq(Events.toCommandsDsl[E, G, B](s.events)))

          case CommandWithConfigAggregate(n, d, conf, r, subs) =>
            CommandWithConfigAggregate(
              n, d, conf, r, subs ++ Seq(Events.toCommandsDsl[E, G, B](s.events)))
        }
      }

    implicit def caseAggregateConfigAdded[G[_]: Plugin, A, B]:
      Case.Aux[Aggregate[G, A], ConfigAdded[G, B], Aggregate[G, B]] =

      at[Aggregate[G, A], ConfigAdded[G, B]] {
        case (c: Aggregate[G, A], conf: ConfigAdded[G, B]) => c match {
          case CommandAggregate(n, d, r, subs) =>
            CommandWithConfigAggregate[G, B](n, d, conf.config, (a: B) => (), subs)

          case CommandWithConfigAggregate(_, _, _, _, _) =>
            throw new Exception("Invalid state: config added to command with config")
        }
      }

    implicit def caseAggregateConfigRunAdded[G[_]: Plugin, A]:
      Case.Aux[Aggregate[G, A], ConfigRunAdded[A], Aggregate[G, A]] =

      at[Aggregate[G, A], ConfigRunAdded[A]] {
        case (c: Aggregate[G, A], run: ConfigRunAdded[A]) => c match {
          case CommandAggregate(_, _, _, _) =>
            throw new Exception("Invalid state: config run added to command without config")

          case CommandWithConfigAggregate(n, d, conf, _, subs) =>
            CommandWithConfigAggregate[G, A](n, d, conf, run.run, subs)

        }
      }

    implicit def caseAggregateRunAdded[G[_]: Plugin, A]:
      Case.Aux[Aggregate[G, A], RunAdded, Aggregate[G, A]] =

      at[Aggregate[G, A], RunAdded] {
        case (c: Aggregate[G, A], run: RunAdded) => c match {
          case CommandAggregate(n, d, _, subs) =>
            CommandAggregate(n, d, run.run, subs)

          case CommandWithConfigAggregate(n, d, conf, _, subs) =>
            throw new Exception("Invalid state: run added to command with config")

        }
      }

    }
}

case class CommandAggregate[G[_]: Plugin, A]
  (name: String,
   description: Option[String],
   run: () => Unit,
   subcommands: Seq[CommandsDsl[G, Command]])
  extends Aggregate[G, A]

case class CommandWithConfigAggregate[G[_]: Plugin, A]
  (name: String,
   description: Option[String],
   config: ConfigDsl[G, A],
   run: A => Unit,
   subcommands: Seq[CommandsDsl[G, Command]])
  extends Aggregate[G, A]

