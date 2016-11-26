package pavlosgi.freecli.core.dsl.command

import shapeless.{::, HList, HNil}

import pavlosgi.freecli.core.api.command.{PartialCommand, RunCommand}
import pavlosgi.freecli.core.dsl.config.ConfigDsl
import pavlosgi.freecli.core.free.FreeAlternative

sealed trait PartsMerger[H1 <: HList, C1, R1, H2 <: HList, C2, R2] {
  type H_ <: HList
  type C_
  type R_
  type Out = CommandPartsBuilder[H_, C_, R_]

  def apply(h1: CommandPartsBuilder[H1, C1, R1], h2: CommandPartsBuilder[H2, C2, R2]): Out
}

object PartsMerger {
  type Aux[H1 <: HList, C1, R1, H2 <: HList, C2, R2, HOut, COut, ROut] =
   PartsMerger[H1, C1, R1, H2, C2, R2] {
     type H_ = HOut
     type C_ = COut
     type R_ = ROut
   }

  implicit def canMergeConfigToGenericRun[C1, R1, C2, R2] =
    new PartsMerger[ConfigDsl[C1] :: HNil, C1, R1, RunCommand[R2] :: HNil, C2, R2] {
      type H_ = ConfigDsl[C1] :: RunCommand[R2] :: HNil
      type C_ = C1
      type R_ = R2

      def apply(
        h1: CommandPartsBuilder[ConfigDsl[C1] :: HNil, C1, R1],
        h2: CommandPartsBuilder[RunCommand[R2] :: HNil, C2, R2]):
        Out = {

        CommandPartsBuilder[ConfigDsl[C1] :: RunCommand[R2] :: HNil, C1, R2](
          h1.list.head :: h2.list.head :: HNil)
      }
    }

  implicit def canMergeConfigToPartial[C1, R1, C2, R2] =
    new PartsMerger[ConfigDsl[C1] :: HNil, C1, R1, CommandDsl[PartialCommand[R2]] :: HNil, C2, R2] {
      type H_ = ConfigDsl[C1] :: CommandDsl[PartialCommand[R2]] :: HNil
      type C_ = C1
      type R_ = R2

      def apply(
        h1: CommandPartsBuilder[ConfigDsl[C1] :: HNil, C1, R1],
        h2: CommandPartsBuilder[CommandDsl[PartialCommand[R2]] :: HNil, C2, R2]):
        Out = {

        CommandPartsBuilder[ConfigDsl[C1] :: CommandDsl[PartialCommand[R2]] :: HNil, C1, R2](
          h1.list.head :: h2.list.head :: HNil)
      }
    }

  implicit def canMergePartialToPartial[R1, R2, RH <: HList](
    implicit ev: ToFromHList[R1, RH],
    ev2: ToFromHList[R2, RH]) =
    new PartsMerger[CommandDsl[PartialCommand[R1]] :: HNil, Unit, R1, CommandDsl[PartialCommand[R2]] :: HNil, Unit, R2] {
      type H_ = CommandDsl[PartialCommand[RH]] :: HNil
      type C_ = Unit
      type R_ = RH

      def apply(
        h1: CommandPartsBuilder[CommandDsl[PartialCommand[R1]] :: HNil, Unit, R1],
        h2: CommandPartsBuilder[CommandDsl[PartialCommand[R2]] :: HNil, Unit, R2]):
        Out = {

        val dsl =
          FreeAlternative.combineK(
            h1.list.head.map(
              partial => PartialCommand[RH](r => partial.f(ev.from(r)))),

            h2.list.head.map(
              partial => PartialCommand[RH](r => partial.f(ev2.from(r)))))

        CommandPartsBuilder(dsl :: HNil)
      }
    }
}

