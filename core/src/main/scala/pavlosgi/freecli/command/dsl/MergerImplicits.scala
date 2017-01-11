package pavlosgi.freecli.command.dsl

import shapeless.{::, HList, HNil}

import pavlosgi.freecli.command.api.{PartialCommand, RunCommand}
import pavlosgi.freecli.config.dsl.ConfigDsl
import pavlosgi.freecli.core.free.FreeAlternative
import pavlosgi.freecli.core.api.{Merger, CanMerge}

trait MergerImplicits {
  implicit def commandDslBuilder2Merger[H <: HList, O, A](
    b: CommandDslBuilder[H, O, A]):
    Merger[CommandDslBuilder[H, O, A]] = {

    Merger(b)
  }

  implicit def canMergeConfigToGenericRun[C1, R1, C2, R2]:
    CanMerge.Aux[
      CommandDslBuilder[ConfigDsl[C1] :: HNil, C1, R1],
      CommandDslBuilder[RunCommand[R2] :: HNil, C2, R2],
      CommandDslBuilder[ConfigDsl[C1] :: RunCommand[R2] :: HNil, C1, R2]] = {

    new CanMerge[
      CommandDslBuilder[ConfigDsl[C1] :: HNil, C1, R1],
      CommandDslBuilder[RunCommand[R2] :: HNil, C2, R2]] {

      type Out = CommandDslBuilder[ConfigDsl[C1] :: RunCommand[R2] :: HNil, C1, R2]

      def apply(
        h1: CommandDslBuilder[ConfigDsl[C1] :: HNil, C1, R1],
        h2: CommandDslBuilder[RunCommand[R2] :: HNil, C2, R2]):
        Out = {

        CommandDslBuilder[ConfigDsl[C1] :: RunCommand[R2] :: HNil, C1, R2](
          h1.list.head :: h2.list.head :: HNil)
      }
    }
  }

  implicit def canMergeConfigToPartial[C1, R1, C2, R2]:
    CanMerge.Aux[
      CommandDslBuilder[ConfigDsl[C1] :: HNil, C1, R1],
      CommandDslBuilder[CommandDsl[PartialCommand[R2]] :: HNil, C2, R2],
      CommandDslBuilder[ConfigDsl[C1] :: CommandDsl[PartialCommand[R2]] :: HNil, C1, R2]] = {

    new CanMerge[
      CommandDslBuilder[ConfigDsl[C1] :: HNil, C1, R1],
      CommandDslBuilder[CommandDsl[PartialCommand[R2]] :: HNil, C2, R2]] {

      type Out = CommandDslBuilder[ConfigDsl[C1] :: CommandDsl[PartialCommand[R2]] :: HNil, C1, R2]

      def apply(
        h1: CommandDslBuilder[ConfigDsl[C1] :: HNil, C1, R1],
        h2: CommandDslBuilder[CommandDsl[PartialCommand[R2]] :: HNil, C2, R2]):
        Out = {

        CommandDslBuilder[ConfigDsl[C1] :: CommandDsl[PartialCommand[R2]] :: HNil, C1, R2](
          h1.list.head :: h2.list.head :: HNil)
      }
    }
  }

  implicit def canMergePartialToPartial[R1, R2, RH <: HList](
    implicit ev: ToFromHList[R1, RH],
    ev2: ToFromHList[R2, RH]):
    CanMerge.Aux[
      CommandDslBuilder[CommandDsl[PartialCommand[R1]] :: HNil, Unit, R1],
      CommandDslBuilder[CommandDsl[PartialCommand[R2]] :: HNil, Unit, R2],
      CommandDslBuilder[CommandDsl[PartialCommand[RH]] :: HNil, Unit, RH]] = {

    new CanMerge[
      CommandDslBuilder[CommandDsl[PartialCommand[R1]] :: HNil, Unit, R1],
      CommandDslBuilder[CommandDsl[PartialCommand[R2]] :: HNil, Unit, R2]] {

      type Out = CommandDslBuilder[CommandDsl[PartialCommand[RH]] :: HNil, Unit, RH]

      def apply(
        h1: CommandDslBuilder[CommandDsl[PartialCommand[R1]] :: HNil, Unit, R1],
        h2: CommandDslBuilder[CommandDsl[PartialCommand[R2]] :: HNil, Unit, R2]):
        Out = {

        val dsl =
          FreeAlternative.combineK(
            h1.list.head.map(
              partial => PartialCommand[RH](r => partial.f(ev.from(r)))),

            h2.list.head.map(
              partial => PartialCommand[RH](r => partial.f(ev2.from(r)))))

        CommandDslBuilder(dsl :: HNil)
      }
    }
  }
}

