package pavlosgi.freecli.core.command.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.{Diff, Prepend, Selector}

import pavlosgi.freecli.core.command.api.{Algebra, Command}
import pavlosgi.freecli.core.dsl.config.ConfigDsl
import pavlosgi.freecli.core.command.dsl.CommandFieldBuilder.CanProduceCommandField
import pavlosgi.freecli.core.api.config.Description

private [command] class PartialCommandDsl[H <: HList, Conf, Run](val list: H) {

  def -?(
    description: String)
   (implicit ev: Prepend[H, Option[Description] :: HNil],
    ev2: NotContainsConstraint[H, Option[Description]]):
    PartialCommandDsl[ev.Out, Conf, Run] =

    new PartialCommandDsl[ev.Out, Conf, Run](this.list :+ Option(Description(description)))

  def apply[T <: HList](
    commandConfig: T)
   (implicit ev: Prepend[H, T]):
    PartialCommandDsl[ev.Out, Conf, Run] = {

    new PartialCommandDsl(list ++ commandConfig)
  }
}

object PartialCommandDsl {
  case class PartialCommandConfiguration[H <: HList, Conf](list: H)
  object PartialCommandConfiguration {
    def takes[T](config: ConfigDsl[T :: HNil]) = config :: HNil

    def runs[T](f: T => Unit) = CommandRun[T](f) :: HNil

    def runs(f: => Unit) = CommandRun[Unit](_ => f) :: HNil
  }

  case class CommandRun[T](run: T => Unit)

  private[command] class Subcommands[Conf] {
    def apply[H <: HList](f: ConfigDsl[Conf :: HNil] => CommandDsl[Command]) =
      f :: HNil

    def apply[H <: HList](f: CommandDsl[Command]) = f :: HNil
  }

  object Subcommands {
    def apply[T] = new Subcommands[T]
  }

  implicit class Merger[H <: HList](val c: H) {
    def ::[L <: HList](
      m: Merger[L])
     (implicit ev: Prepend[H, L]) =
      c ++ m.c
  }

  sealed trait CanProduceCommandDsl[H <: HList, Conf, Run] {
    def apply(partial: PartialCommandDsl[H, Conf, Run]): CommandDsl[Command]
  }

  object CanProduceCommandDsl {
    implicit def canProduceCommandDsl[H <: HList, Rem <: HList](
      implicit ev: CanProduceCommandField.Aux[H, Rem],
      ev2: Selector[Rem, CommandRun[Unit]],
      ev3: Diff.Aux[Rem, CommandRun[Unit] :: HNil, HNil]):
      CanProduceCommandDsl[H, Unit, Unit] = {

      new CanProduceCommandDsl[H, Unit, Unit] {
        override def apply(partial: PartialCommandDsl[H, Unit, Unit]): CommandDsl[Command] = {
          val (field, remaining) = ev(partial.list)
          val run = ev2(remaining)

          new CommandDsl[Command] {
            override def apply[F[_] : Algebra]: F[Command] =
              implicitly[Algebra[F]].cmd(field, run.run(()))
          }
        }
      }
    }

    implicit def canProduceCommandDslWithSubcommands[H <: HList, Rem <: HList](
      implicit ev: CanProduceCommandField.Aux[H, Rem],
      ev2: Selector[Rem, CommandDsl[Command]],
      ev4: Diff.Aux[Rem, CommandDsl[Command] :: HNil, HNil]):
      CanProduceCommandDsl[H, Unit, Unit] = {

      new CanProduceCommandDsl[H, Unit, Unit] {
        override def apply(partial: PartialCommandDsl[H, Unit, Unit]): CommandDsl[Command] = {
          val (field, remaining) = ev(partial.list)
          val subs = ev2(remaining)

          new CommandDsl[Command] {
            override def apply[F[_] : Algebra]: F[Command] =
              implicitly[Algebra[F]].cmdWithSubcommands(field, subs)
          }
        }
      }
    }

    implicit def canProduceCommandDslWithConfig[H <: HList, Rem <: HList, Conf, Run](
      implicit ev: CanProduceCommandField.Aux[H, Rem],
      ev2: Selector[Rem, ConfigDsl[Conf :: HNil]],
      ev3: Selector[Rem, CommandRun[Run]],
      ev4: Generic.Aux[Run, Conf :: HNil],
      ev5: Diff.Aux[Rem, ConfigDsl[Conf :: HNil] :: CommandRun[Run] :: HNil, HNil]):
      CanProduceCommandDsl[H, Conf, Run] = {

      new CanProduceCommandDsl[H, Conf, Run] {
        override def apply(partial: PartialCommandDsl[H, Conf, Run]): CommandDsl[Command] = {
          val (field, remaining) = ev(partial.list)
          val config = ev2(remaining).map(ev4.from)
          val run = ev3(remaining)

          new CommandDsl[Command] {
            override def apply[F[_] : Algebra]: F[Command] = {
              implicit val c = implicitly[Algebra[F]].configAlgebra
              implicitly[Algebra[F]].cmdWithConfig(field, config, run.run)
            }
          }
        }
      }
    }

    implicit def canProduceCommandDslWithConfigAndSubcommands[H <: HList, Rem <: HList, Conf](
      implicit ev: CanProduceCommandField.Aux[H, Rem],
      ev2: Selector[Rem, ConfigDsl[Conf :: HNil]],
      ev3: Selector[Rem, ConfigDsl[Conf :: HNil] => CommandDsl[Command]],
      ev4: Diff.Aux[Rem, ConfigDsl[Conf :: HNil] :: (ConfigDsl[Conf :: HNil] => CommandDsl[Command]) :: HNil, HNil]):
      CanProduceCommandDsl[H, Conf, Unit] = {

      new CanProduceCommandDsl[H, Conf, Unit] {
        override def apply(partial: PartialCommandDsl[H, Conf, Unit]): CommandDsl[Command] = {
          val (field, remaining) = ev(partial.list)
          val config = ev2(remaining)
          val subs = ev3(remaining)(config)

          new CommandDsl[Command] {
            override def apply[F[_] : Algebra]: F[Command] = {
              implicit val c = implicitly[Algebra[F]].configAlgebra
              implicitly[Algebra[F]].cmdWithConfigAndSubcommands(field, config, subs)
            }
          }
        }
      }
    }
  }

  implicit def toCommandDsl[H <: HList, Conf, Run](
    partial: PartialCommandDsl[H, Conf, Run])
   (implicit ev: CanProduceCommandDsl[H, Conf, Run]):
    CommandDsl[Command] = ev(partial)

  implicit def toCommandDslMerger[H <: HList, Conf, Run](
    partial: PartialCommandDsl[H, Conf, Run])
   (implicit ev: CanProduceCommandDsl[H, Conf, Run]):
    CommandDsl.Merger = ev(partial)

}
