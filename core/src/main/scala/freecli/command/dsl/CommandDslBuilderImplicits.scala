package freecli
package command
package dsl

import shapeless._
import shapeless.ops.hlist.{Diff, LeftFolder, Prepend}

import api._
import config.{dsl => C}
import core.api.CanProduce
import core.free.FreeAlternative
import core.poly.toHList

trait CommandDslBuilderImplicits {
  implicit def canProduceCommandDsl1[H <: HList, Run](
    implicit ev: CanProduce.Aux[H, (CommandField, RunCommand[Run] :: HNil)]):
    CanProduce.Aux[
      CommandDslBuilder[H, HNil, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[Run]] :: HNil, HNil, Run]] = {

    new CanProduce[CommandDslBuilder[H, HNil, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[Run]] :: HNil, HNil, Run]

      def apply(
        t: CommandDslBuilder[H, HNil, Run]):
        Out = {

        val (field, rem) = ev(t.list)
        CommandDslBuilder(
          FreeAlternative.lift[Algebra, PartialCommand[Run]](
          PartialCmd[Run, PartialCommand[Run]](field, rem.head.f, identity)) :: HNil)
      }
    }
  }

  implicit def canProduceCommandDsl2[H <: HList, Conf, Run, RunH <: HList, OutRun <: HList](
    implicit ev: CanProduce.Aux[H, (CommandField, C.ConfigDsl[Conf] :: RunCommand[Run] :: HNil)],
    runToFrom: ToFromHList[Run, RunH],
    diff: Diff.Aux[RunH, Conf :: HNil, OutRun],
    prepend: Prepend.Aux[OutRun, Conf :: HNil, RunH]):
    CanProduce.Aux[
      CommandDslBuilder[H, Conf, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[OutRun]] :: HNil, HNil, OutRun]] = {

    new CanProduce[CommandDslBuilder[H, Conf, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[OutRun]] :: HNil, HNil, OutRun]

      def apply(
        t: CommandDslBuilder[H, Conf, Run]):
        Out = {

        val (field, rem) = ev(t.list)
        val run = (c: Conf) => (p: OutRun) => {
          rem.tail.head.f(runToFrom.from(p ++ (c :: HNil)))
        }

        CommandDslBuilder(
          FreeAlternative.lift[Algebra, PartialCommand[OutRun]](
          PartialCmdWithConfig[Conf, OutRun, PartialCommand[OutRun]](
            field,
            rem.head,
            run,
            identity)) :: HNil)
      }
    }
  }

  implicit def canProduceCommandDsl3[H <: HList, Conf, Run](
    implicit ev: CanProduce.Aux[H, (CommandField, C.ConfigDsl[Conf] :: RunCommand[Run] :: HNil)],
    equal: Conf =:= Run):
    CanProduce.Aux[
      CommandDslBuilder[H, Conf, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[HNil]] :: HNil, HNil, HNil]] = {

    new CanProduce[CommandDslBuilder[H, Conf, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[HNil]] :: HNil, HNil, HNil]

      def apply(
        t: CommandDslBuilder[H, Conf, Run]):
        Out = {

        val (field, rem) = ev(t.list)
        val run = (c: Conf) => (p: HNil) => {
          rem.tail.head.f(equal(c))
        }

        CommandDslBuilder(
          FreeAlternative.lift[Algebra, PartialCommand[HNil]](
          PartialCmdWithConfig[Conf, HNil, PartialCommand[HNil]](
            field,
            rem.head,
            run,
            identity)) :: HNil)
      }
    }
  }

  implicit def canProduceComposedCommandDsl[H <: HList, Conf, ConfH <: HList, Run, RunH <: HList, OutRun <: HList](
    implicit ev: CanProduce.Aux[H, (CommandField, C.ConfigDsl[Conf] :: CommandDsl[PartialCommand[Run]] :: HNil)],
    runToFrom: ToFromHList[Run, RunH],
    confToHList: LeftFolder.Aux[Conf :: HNil, HNil, toHList.type, ConfH],
    diff: Diff.Aux[RunH, ConfH, OutRun],
    prepend: Prepend.Aux[OutRun, ConfH, RunH]):
    CanProduce.Aux[
      CommandDslBuilder[H, Conf, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[OutRun]] :: HNil, HNil, OutRun]] = {

    new CanProduce[CommandDslBuilder[H, Conf, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[OutRun]] :: HNil, HNil, OutRun]

      def apply(
        t: CommandDslBuilder[H, Conf, Run]):
        Out = {

        val (field, rem) = ev(t.list)
        val subs = rem.tail.head.map { partial =>
            (c: Conf) => PartialCommand[OutRun](
              p => partial.f(runToFrom.from(p ++ confToHList(c :: HNil, HNil))))
          }

        CommandDslBuilder(
          FreeAlternative.lift[Algebra, PartialCommand[OutRun]](
            PartialParentCmdWithConfig[Conf, OutRun, PartialCommand[OutRun]](
              field,
              rem.head,
              subs,
              identity)) :: HNil)
      }
    }
  }

  implicit def canProduceComposedCommandDsl2[H <: HList, Run](
    implicit ev: CanProduce.Aux[H, (CommandField, CommandDsl[PartialCommand[Run]] :: HNil)]):
    CanProduce.Aux[
      CommandDslBuilder[H, HNil, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[Run]] :: HNil, HNil, Run]] = {

    new CanProduce[CommandDslBuilder[H, HNil, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[Run]] :: HNil, HNil, Run]

      def apply(
        t: CommandDslBuilder[H, HNil, Run]):
        Out = {

        val (field, rem) = ev(t.list)
        CommandDslBuilder(
          FreeAlternative.lift[Algebra, PartialCommand[Run]](
            PartialParentCmd[Run, PartialCommand[Run]](field, rem.head, identity)) :: HNil)
      }
    }
  }

  implicit def flattenBuilderCommandDsl[P, F](
    b: CommandDslBuilder[CommandDsl[PartialCommand[P]] :: HNil, HNil, P])(
    implicit ev: FlattenPartialCommand[P]):
    FreeAlternative[Algebra, Command] = {

    b.list.head.map(ev.apply)
  }
}
