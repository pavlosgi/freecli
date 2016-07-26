package pavlosgi
package freecli
package commands
package dsl

import scala.annotation.implicitNotFound

import cats.syntax.all._
import cats.Alternative
import shapeless.ops.hlist.{LeftFolder, Prepend, Selector}
import shapeless.{::, HList, HNil, NotContainsConstraint}

import algebra._
import config.algebra.Plugin
import config.dsl.ConfigDsl

sealed trait Event

object Event {
  implicit def ev2Events[E <: Event](e: E): Events[E :: HNil] = {
    Events(e :: HNil)
  }
}

@implicitNotFound("Implicit not found: shapeless.EventConstraint[${E}${H}]: " +
                  "Please ensure that the event ${E} can come after the existing series of events")
sealed trait EventConstraint[E <: Event, H <: HList]

case class NameAdded(name: String) extends Event
object NameAdded {
  implicit def constraint[H <: HList]
    (implicit ev: NotContainsConstraint[H, NameAdded]):
    EventConstraint[NameAdded, H] = {

    new EventConstraint[NameAdded, H] {}
  }

  implicit class NameAddedOps[G[_]: Plugin](n: NameAdded) {
    def dsl: CommandsDsl[G, Command] = Events.toCommandsDsl(n)
  }

  implicit class EventOps(e: NameAdded) {
    def apply[H <: HList]
      (newEvents: Events[H])
      (implicit eventConstraint: EventConstraint[NameAdded, H]):
      Events[NameAdded :: H] = {

      Events(e :: newEvents.events)
    }
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

  implicit def toCommandsDsl[E <: HList, G[_]: Plugin, B]
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