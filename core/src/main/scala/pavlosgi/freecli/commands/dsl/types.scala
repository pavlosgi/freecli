package pavlosgi
package freecli
package commands
package dsl

import scala.annotation.implicitNotFound

import cats.syntax.all._
import cats.Alternative
import shapeless.ops.hlist.{LeftFolder, Prepend, Selector}
import shapeless.{::, HList, HNil, NotContainsConstraint, Poly2}

import algebra._
import config.algebra.Plugin
import config.dsl.ConfigDsl

trait Types {
  type CommandsDsl[G[_], A] = ApplyCommandAlgebra[G, A]

  implicit def alternativeDsl[G[_]: Plugin]: Alternative[CommandsDsl[G, ?]] =
    new Alternative[CommandsDsl[G, ?]] {
      override def combineK[A](x: CommandsDsl[G, A],
                               y: CommandsDsl[G, A]): CommandsDsl[G, A] = {

        new CommandsDsl[G, A] {
          override def apply[F[_] : CommandAlgebra[?[_], G]]: F[A] =
            x.apply[F].combineK(y.apply[F])
        }
      }

      override def pure[A](x: A): CommandsDsl[G, A] = new CommandsDsl[G, A] {
        def apply[F[_]: CommandAlgebra[?[_], G]] = x.pure[F]
      }

      override def ap[A, B](ff: CommandsDsl[G, (A) => B])
                           (fa: CommandsDsl[G, A]): CommandsDsl[G, B] = {

        new CommandsDsl[G, B] {
          override def apply[F[_] : CommandAlgebra[?[_], G]]: F[B] =
            ff.apply[F].ap(fa.apply[F])
        }
      }

      override def empty[A]: CommandsDsl[G, A] = new CommandsDsl[G, A] {
        override def apply[F[_]: CommandAlgebra[?[_], G]]: F[A] =
          implicitly[Alternative[F]].empty
      }
    }
}

@implicitNotFound("Implicit not found: shapeless.EventConstraint[${E}${H}]: " +
                  "Please ensure that the event ${E} can come after the existing series of events")
sealed trait EventConstraint[E <: Event, H <: HList]

sealed trait Event
object Event {
  implicit def ev2Events[E <: Event](e: E): Events[E :: HNil] = {
    Events(e :: HNil)
  }

  implicit class EventOps[E <: Event](e: E) {
    def apply[H <: HList]
      (newEvents: Events[H])
      (implicit eventConstraint: EventConstraint[E, H]):
      Events[E :: H] = {

      Events(e :: newEvents.events)
    }
  }
}

case class NameAdded(name: String) extends Event
object NameAdded {
  implicit def constraint[H <: HList]
    (implicit ev: NotContainsConstraint[H, NameAdded]):
    EventConstraint[NameAdded, H] = {

    new EventConstraint[NameAdded, H] {}
  }

  implicit class NameAddedOps[G[_]: Plugin](n: NameAdded) {
    def dsl: CommandsDsl[G, Command] = Events.toCommandDsl(n)
  }
}

case class DescriptionAdded(description: String) extends Event
object DescriptionAdded {
  implicit def constraint[H <: HList]
    (implicit ev: NotContainsConstraint[H, DescriptionAdded]):
    EventConstraint[DescriptionAdded, H] = {

    new EventConstraint[DescriptionAdded, H] {}
  }
}

case class CommandAdded[E <: HList](events: Events[E]) extends Event
object CommandAdded {
  implicit def constraint[E <: HList, H <: HList]: EventConstraint[CommandAdded[E], H] = {
    new EventConstraint[CommandAdded[E], H] {}
  }
}

case class ConfigAdded[G[_]: Plugin, A](config: ConfigDsl[G, A]) extends Event
object ConfigAdded {
  implicit def constraint[G[_]: Plugin, H <: HList, E <: HList, A]
    (implicit ev: NotContainsConstraint[H, ConfigAdded[G, _]],
     ev2: NotContainsConstraint[H, RunAdded],
     ev3: NotContainsConstraint[H, CommandAdded[E]]):
    EventConstraint[ConfigAdded[G, A], H] = {

    new EventConstraint[ConfigAdded[G, A], H] {}
  }
}

case class RunAdded(run: () => Unit) extends Event
object RunAdded {
  implicit def constraint[G[_]: Plugin, H <: HList, E <: HList, A]
    (implicit ev: NotContainsConstraint[H, RunAdded],
     ev2: NotContainsConstraint[H, ConfigRunAdded[_]],
     ev3: NotContainsConstraint[H, ConfigAdded[G, _]],
     ev4: NotContainsConstraint[H, CommandAdded[E]]):
    EventConstraint[RunAdded, H] = {

    new EventConstraint[RunAdded, H] {}
  }
}

case class ConfigRunAdded[A](run: A => Unit) extends Event
object ConfigRunAdded {
  implicit def constraint[G[_]: Plugin, H <: HList, A]
    (implicit ev: NotContainsConstraint[H, ConfigRunAdded[_]],
     ev2: NotContainsConstraint[H, RunAdded],
     ev3: Selector[H, ConfigAdded[G, A]]):
    EventConstraint[ConfigRunAdded[A], H] = {

    new EventConstraint[ConfigRunAdded[A], H] {}
  }
}

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
              n, d, r, subs ++ Seq(Events.toCommandDsl[E, G, B](s.events)))

          case CommandWithConfigAggregate(n, d, conf, r, subs) =>
            CommandWithConfigAggregate(
              n, d, conf, r, subs ++ Seq(Events.toCommandDsl[E, G, B](s.events)))
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

case class Events[E <: HList](events: E) {
  def ~[H <: HList, Ev <: Event]
    (event: Ev)
    (implicit ev: EventConstraint[Ev, E],
     prepend : Prepend[E, Ev :: HNil]):
    Events[prepend.Out] = {

    Events(events :+ event)
  }
}

object Events {
  import Aggregate._

  implicit def toCommandDsl[E <: HList, G[_]: Plugin, B]
    (events: Events[NameAdded :: E])
    (implicit ev: LeftFolder.Aux[NameAdded :: E, Aggregate[G, Unit], aggregate.type, Aggregate[G, B]]):
    CommandsDsl[G, Command] = {

    new CommandsDsl[G, Command] {
      def apply[F[_]: CommandAlgebra[?[_], G]]: F[Command] = {
        val nameAdded = events.events.head
        val init: Aggregate[G, Unit] =
          CommandAggregate[G, Unit](nameAdded.name, None, () => (), Seq.empty)

        events.events.foldLeft(init)(aggregate) match {
          case CommandAggregate(name, description, run, subs) =>
            implicitly[CommandAlgebra[F, G]].cmd(
              CommandField(CommandFieldName(name), description.map(Description.apply)),
              run(),
              subs.toList
              .foldLeft(implicitly[Alternative[CommandsDsl[G, ?]]].empty[Command]) {
                case (p, n) => p.combineK(n)
              })

          case CommandWithConfigAggregate(name, description, config, run, subs) =>
            implicitly[CommandAlgebra[F, G]].cmdWithConfig(
              CommandField(CommandFieldName(name), description.map(Description.apply)),
              config,
              run,
              subs.toList
              .foldLeft(implicitly[Alternative[CommandsDsl[G, ?]]].empty[Command]) {
                case (p, n) => p.combineK(n)
              })
        }
      }
    }
  }
}