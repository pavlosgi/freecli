package pavlosgi.freecli.core.dsl.command

import shapeless._
import shapeless.ops.hlist.{Diff, Prepend}

import pavlosgi.freecli.core.api.command._
import pavlosgi.freecli.core.dsl.config.ConfigDsl
import pavlosgi.freecli.core.free.FreeAlternative

sealed trait CanProduceDsl[H <: HList, Conf, Run] {
  type OutConf
  type OutRun

  type A = PartialCommand[OutRun]

  type Out = CommandDslBuilder[CommandDsl[A] :: HNil, OutConf, OutRun]
  def apply(field: CommandField, parts: CommandDslBuilder[H, Conf, Run]): Out
}

object CanProduceDsl {
  type Aux[H <: HList, Conf, Run, OutConf_, OutRun_] =
    CanProduceDsl[H, Conf, Run] {
      type OutConf = OutConf_
      type OutRun = OutRun_
    }

  implicit def canProducePartialFromRun[P, Run] =
    new CanProduceDsl[RunCommand[Run] :: HNil, Unit, Run] {
      type OutConf = Unit
      type OutRun = Run

      def apply(
        field: CommandField,
        parts: CommandDslBuilder[RunCommand[Run] :: HNil, Unit, Run]):
        Out = {

        CommandDslBuilder(
          FreeAlternative.lift[Algebra, A](
            PartialCmd[OutRun, A](field, parts.list.head.f, identity)) :: HNil)
      }
    }

  implicit def canProducePartialFromConfigAndRun[Conf, Run, RunH <: HList, OutRun_ <: HList](
    implicit runToFrom: ToFromHList[Run, RunH],
    diff: Diff.Aux[RunH, Conf :: HNil, OutRun_],
    prepend: Prepend.Aux[OutRun_, Conf :: HNil, RunH]) =

    new CanProduceDsl[ConfigDsl[Conf] :: RunCommand[Run] :: HNil, Conf, Run] {
      type OutConf = Unit
      type OutRun = OutRun_

      def apply(
        field: CommandField,
        parts: CommandDslBuilder[ConfigDsl[Conf] :: RunCommand[Run] :: HNil, Conf, Run]):
        Out = {

        val run = (c: Conf) => (p: OutRun_) => {
          parts.list.tail.head.f(runToFrom.from(p ++ (c :: HNil)))
        }

        CommandDslBuilder(
          FreeAlternative.lift[Algebra, A](
            PartialCmdWithConfig[Conf, OutRun_, A](
              field,
              parts.list.head,
              run,
              identity)) :: HNil)
      }
    }

  implicit def canProducePartialFromConfigAndRunSame[Conf, Run](
    implicit equality: Conf =:= Run) =

    new CanProduceDsl[ConfigDsl[Conf] :: RunCommand[Run] :: HNil, Conf, Run] {
      type OutConf = Unit
      type OutRun = HNil

      def apply(
        field: CommandField,
        parts: CommandDslBuilder[ConfigDsl[Conf] :: RunCommand[Run] :: HNil, Conf, Run]):
        Out = {

        val run = (c: Conf) => (p: HNil) => {
          parts.list.tail.head.f(equality(c))
        }

        CommandDslBuilder(
          FreeAlternative.lift[Algebra, A](
            PartialCmdWithConfig[Conf, HNil, A](
              field,
              parts.list.head,
              run,
              identity)) :: HNil)
      }
    }

  implicit def canProducePartialFromConfigAndPartial[Conf, Run, RunH <: HList, OutRun_ <: HList](
    implicit runToFrom: ToFromHList[Run, RunH],
    ev: Diff.Aux[RunH, Conf :: HNil, OutRun_],
    ev2: Prepend.Aux[OutRun_, Conf :: HNil, RunH]) =

    new CanProduceDsl[ConfigDsl[Conf] :: CommandDsl[PartialCommand[Run]] :: HNil, Conf, Run] {
      type OutConf = Unit
      type OutRun = OutRun_

      def apply(
        field: CommandField,
        parts: CommandDslBuilder[ConfigDsl[Conf] :: CommandDsl[PartialCommand[Run]] :: HNil, Conf, Run]):
        Out = {

        val subs = parts.list.tail.head.map { partial =>
            (c: Conf) => PartialCommand[OutRun_](
              p => partial.f(runToFrom.from(p ++ (c :: HNil))))
          }

        CommandDslBuilder(FreeAlternative.lift[Algebra, A](
          PartialParentCmdWithConfig[Conf, OutRun_, A](
            field,
            parts.list.head,
            subs,
            identity)) :: HNil)
      }
    }

  implicit def canProducePartialFromPartial[Run] =
    new CanProduceDsl[CommandDsl[PartialCommand[Run]] :: HNil, Unit, Run] {
      type OutConf = Unit
      type OutRun = Run

      def apply(
        field: CommandField,
        parts: CommandDslBuilder[CommandDsl[PartialCommand[Run]] :: HNil, Unit, Run]):
        Out = {

        CommandDslBuilder(FreeAlternative.lift[Algebra, A](
          PartialParentCmd[OutRun, A](field, parts.list.head, identity)) :: HNil)
      }
    }
}
