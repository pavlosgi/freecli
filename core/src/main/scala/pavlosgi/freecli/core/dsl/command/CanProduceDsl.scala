package pavlosgi.freecli.core.dsl.command

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.{Diff, Prepend}

import pavlosgi.freecli.core.api.command.{Algebra, CommandField, PartialCommand, RunCommand}
import pavlosgi.freecli.core.dsl.options.OptionsDsl
import pavlosgi.freecli.core.api.options.{Algebra => OptionsAlgebra}

sealed trait CanProduceDsl[H <: HList, Conf, Run] {
  type Dsl
  type C_
  type R_

  type Out = CommandPartsBuilder[CommandDsl[Dsl] :: HNil, C_, R_]
  def apply(field: CommandField, parts: CommandPartsBuilder[H, Conf, Run]): Out
}

object CanProduceDsl {
  type Aux[H <: HList, Conf, Run, Dsl1, Conf1, Run1] =
    CanProduceDsl[H, Conf, Run] {
      type Dsl = Dsl1
      type C_ = Conf1
      type R_ = Run1
    }

  implicit def canProducePartialFromRun[P, Run] =
    new CanProduceDsl[RunCommand[Run] :: HNil, Unit, Run] {
      type Dsl = PartialCommand[Run]
      type C_ = Unit
      type R_ = Run

      def apply(
        field: CommandField,
        parts: CommandPartsBuilder[RunCommand[Run] :: HNil, Unit, Run]):
        Out = {

        val dsl =
          new CommandDsl[PartialCommand[Run]] {
            override def apply[F[_], C[_]](
              implicit ev: OptionsAlgebra[C],
              ev2: Algebra[F, C]):
              F[PartialCommand[Run]] = {

              implicitly[Algebra[F, C]].partialCmd(field, parts.list.head.f)
            }
          }

        CommandPartsBuilder(dsl :: HNil)
      }
    }

  implicit def canProducePartialFromConfigAndRun[P <: HList, Conf, Run, RunH <: HList](
    implicit runToFrom: ToFromHList[Run, RunH],
    diff: Diff.Aux[RunH, Conf :: HNil, P],
    prepend: Prepend.Aux[P, Conf :: HNil, RunH]) =

    new CanProduceDsl[OptionsDsl[Conf] :: RunCommand[Run] :: HNil, Conf, Run] {
      type Dsl = PartialCommand[P]
      type C_ = Unit
      type R_ = P

      def apply(
        field: CommandField,
        parts: CommandPartsBuilder[OptionsDsl[Conf] :: RunCommand[Run] :: HNil, Conf, Run]):
        Out = {

        val dsl =
          new CommandDsl[PartialCommand[P]] {
            override def apply[F[_], C[_]](
              implicit ev: OptionsAlgebra[C],
              ev2: Algebra[F, C]):
              F[PartialCommand[P]] = {

              val run = (c: Conf) => (p: P) => {
                parts.list.tail.head.f(runToFrom.from(p ++ (c :: HNil)))
              }

              implicitly[Algebra[F, C]].partialCmdWithConfig(field, parts.list.head, run)
            }
          }

        CommandPartsBuilder(dsl :: HNil)
      }
    }

  implicit def canProducePartialFromConfigAndRunSame[Conf, Run](
    implicit equality: Conf =:= Run) =

    new CanProduceDsl[OptionsDsl[Conf] :: RunCommand[Run] :: HNil, Conf, Run] {
      type Dsl = PartialCommand[HNil]
      type C_ = Unit
      type R_ = HNil

      def apply(
        field: CommandField,
        parts: CommandPartsBuilder[OptionsDsl[Conf] :: RunCommand[Run] :: HNil, Conf, Run]):
        Out = {

        val dsl =
          new CommandDsl[PartialCommand[HNil]] {
            override def apply[F[_], C[_]](
              implicit ev: OptionsAlgebra[C],
              ev2: Algebra[F, C]):
              F[PartialCommand[HNil]] = {

              val run = (c: Conf) => (p: HNil) => {
                parts.list.tail.head.f(equality(c))
              }

              implicitly[Algebra[F, C]].partialCmdWithConfig(field, parts.list.head, run)
            }
          }

        CommandPartsBuilder(dsl :: HNil)
      }
    }

  implicit def canProducePartialFromConfigAndPartial[P <: HList, Conf, Run, RunH <: HList](
    implicit runToFrom: ToFromHList[Run, RunH],
    ev: Diff.Aux[RunH, Conf :: HNil, P],
    ev2: Prepend.Aux[P, Conf :: HNil, RunH]) =

    new CanProduceDsl[OptionsDsl[Conf] :: CommandDsl[PartialCommand[Run]] :: HNil, Conf, Run] {
      type Dsl = PartialCommand[P]
      type C_ = Unit
      type R_ = P

      def apply(
        field: CommandField,
        parts: CommandPartsBuilder[OptionsDsl[Conf] :: CommandDsl[PartialCommand[Run]] :: HNil, Conf, Run]):
        Out = {

        val dsl =
          new CommandDsl[PartialCommand[P]] {
            override def apply[F[_], C[_]](
              implicit confAlg: OptionsAlgebra[C],
              alg: Algebra[F, C]):
              F[PartialCommand[P]] = {

              val subs = parts.list.tail.head.map { partial =>
                (c: Conf) => PartialCommand[P](
                  p => partial.f(runToFrom.from(p ++ (c :: HNil))))
              }

              implicitly[Algebra[F, C]].partialParentCmdWithConfig(field, parts.list.head, subs)
            }
          }

        CommandPartsBuilder(dsl :: HNil)
      }
    }

  implicit def canProducePartialFromPartial[Run] =
    new CanProduceDsl[CommandDsl[PartialCommand[Run]] :: HNil, Unit, Run] {
      type Dsl = PartialCommand[Run]
      type C_ = Unit
      type R_ = Run

      def apply(
        field: CommandField,
        parts: CommandPartsBuilder[CommandDsl[PartialCommand[Run]] :: HNil, Unit, Run]):
        Out = {

        val dsl =
          new CommandDsl[PartialCommand[Run]] {
            override def apply[F[_], C[_]](
              implicit ev: OptionsAlgebra[C],
              ev2: Algebra[F, C]):
              F[PartialCommand[Run]] = {

              val subs = parts.list.head
              implicitly[Algebra[F, C]].partialParentCmd(field, subs)
            }
          }

        CommandPartsBuilder(dsl :: HNil)
      }
    }
}
