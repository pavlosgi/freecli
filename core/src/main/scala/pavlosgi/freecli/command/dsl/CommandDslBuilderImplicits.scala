package pavlosgi.freecli.command.dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Prepend}

import pavlosgi.freecli.command.api._
import pavlosgi.freecli.config.{dsl => C}
import pavlosgi.freecli.core.api.CanProduce
import pavlosgi.freecli.core.free.FreeAlternative

trait CommandDslBuilderImplicits {
  implicit def canProducePartialFromRun[H <: HList, Run](
    implicit ev: CanProduce.Aux[H, (CommandField, RunCommand[Run] :: HNil)]):
    CanProduce.Aux[
      CommandDslBuilder[H, Unit, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[Run]] :: HNil, Unit, Run]] = {

    new CanProduce[CommandDslBuilder[H, Unit, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[Run]] :: HNil, Unit, Run]

      def apply(
        t: CommandDslBuilder[H, Unit, Run]):
        Out = {

        val (field, rem) = ev(t.list)
        CommandDslBuilder(
          FreeAlternative.lift[Algebra, PartialCommand[Run]](
          PartialCmd[Run, PartialCommand[Run]](field, rem.head.f, identity)) :: HNil)
      }
    }
  }

  implicit def canProducePartialFromConfigAndRun[H <: HList, Conf, Run, RunH <: HList, OutRun <: HList](
    implicit ev: CanProduce.Aux[H, (CommandField, C.ConfigDsl[Conf] :: RunCommand[Run] :: HNil)],
    runToFrom: ToFromHList[Run, RunH],
    diff: Diff.Aux[RunH, Conf :: HNil, OutRun],
    prepend: Prepend.Aux[OutRun, Conf :: HNil, RunH]):
    CanProduce.Aux[
      CommandDslBuilder[H, Conf, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[OutRun]] :: HNil, Unit, OutRun]] = {

    new CanProduce[CommandDslBuilder[H, Conf, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[OutRun]] :: HNil, Unit, OutRun]

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

  implicit def canProducePartialFromConfigAndRunSame[H <: HList, Conf, Run](
    implicit ev: CanProduce.Aux[H, (CommandField, C.ConfigDsl[Conf] :: RunCommand[Run] :: HNil)],
    equal: Conf =:= Run):
    CanProduce.Aux[
      CommandDslBuilder[H, Conf, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[HNil]] :: HNil, Unit, HNil]] = {

    new CanProduce[CommandDslBuilder[H, Conf, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[HNil]] :: HNil, Unit, HNil]

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


  implicit def canProducePartialFromConfigAndPartial[H <: HList, Conf, Run, RunH <: HList, OutRun <: HList](
    implicit ev: CanProduce.Aux[H, (CommandField, C.ConfigDsl[Conf] :: CommandDsl[PartialCommand[Run]] :: HNil)],
    runToFrom: ToFromHList[Run, RunH],
    diff: Diff.Aux[RunH, Conf :: HNil, OutRun],
    prepend: Prepend.Aux[OutRun, Conf :: HNil, RunH]):
    CanProduce.Aux[
      CommandDslBuilder[H, Conf, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[OutRun]] :: HNil, Unit, OutRun]] = {

    new CanProduce[CommandDslBuilder[H, Conf, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[OutRun]] :: HNil, Unit, OutRun]

      def apply(
        t: CommandDslBuilder[H, Conf, Run]):
        Out = {

        val (field, rem) = ev(t.list)
        val subs = rem.tail.head.map { partial =>
            (c: Conf) => PartialCommand[OutRun](
              p => partial.f(runToFrom.from(p ++ (c :: HNil))))
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

  implicit def canProducePartialFromPartial[H <: HList, Run](
    implicit ev: CanProduce.Aux[H, (CommandField, CommandDsl[PartialCommand[Run]] :: HNil)]):
    CanProduce.Aux[
      CommandDslBuilder[H, Unit, Run],
      CommandDslBuilder[CommandDsl[PartialCommand[Run]] :: HNil, Unit, Run]] = {

    new CanProduce[CommandDslBuilder[H, Unit, Run]] {
      type Out = CommandDslBuilder[CommandDsl[PartialCommand[Run]] :: HNil, Unit, Run]

      def apply(
        t: CommandDslBuilder[H, Unit, Run]):
        Out = {

        val (field, rem) = ev(t.list)
        CommandDslBuilder(
          FreeAlternative.lift[Algebra, PartialCommand[Run]](
            PartialParentCmd[Run, PartialCommand[Run]](field, rem.head, identity)) :: HNil)
      }
    }
  }

  implicit def flattenBuilderCommandDsl[P, F](
    b: CommandDslBuilder[CommandDsl[PartialCommand[P]] :: HNil, Unit, P])(
    implicit ev: FlattenPartialCommand[P]):
    FreeAlternative[Algebra, Command] = {

    b.list.head.map(ev.apply)
  }
}
