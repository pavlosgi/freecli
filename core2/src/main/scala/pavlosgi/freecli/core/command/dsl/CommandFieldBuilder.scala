package pavlosgi.freecli.core.command.dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.command.api.{CommandField, CommandFieldName, Description}

case class PartialCommandField(
  name: Option[CommandFieldName],
  description: Option[Description])

object CommandFieldBuilder {
  type CommandFieldTypes =
    CommandFieldName :: Option[Description] :: HNil

  abstract class CanProduceCommandField[H <: HList] {
    type IOut <: HList
    type DOut <: HList

    val intersection: Intersection.Aux[H, CommandFieldTypes, IOut]
    val folder: LeftFolder.Aux[IOut, PartialCommandField, aggregate.type, CommandField]
    val diff: Diff.Aux[H, IOut, DOut]

    def apply(list: H): (CommandField, DOut) = {
      val inters = intersection.apply(list)
      val field = getCommandField(inters)(folder)
      val remaining = diff.apply(list)

      field -> remaining
    }
  }

  object CanProduceCommandField {
    type Aux[H <: HList, Out <: HList] = CanProduceCommandField[H] {
      type DOut = Out
    }

    implicit def canProduceCommandField[H <: HList, Out0 <: HList, Out1 <: HList](
      implicit ev: Intersection.Aux[H, CommandFieldTypes, Out0],
      ev2: LeftFolder.Aux[Out0, PartialCommandField, aggregate.type, CommandField],
      ev3: Diff.Aux[H, Out0, Out1]) = {

      new CanProduceCommandField[H] {
        override type IOut = Out0
        override type DOut = Out1
        override val intersection = ev
        override val folder = ev2
        override val diff = ev3
      }

    }
  }

  def getCommandField[H <: HList](
    list: H)
   (implicit ev: LeftFolder.Aux[H, PartialCommandField, aggregate.type, CommandField]):
    ev.Out = {

    list.foldLeft(PartialCommandField(None, None))(aggregate)
  }

  object aggregate extends Poly2 {
    implicit def caseCommandFieldCommandFieldName:
      Case.Aux[CommandField, CommandFieldName, CommandField] =

      at[CommandField, CommandFieldName] {
        case (field, n: CommandFieldName) =>
          field.copy(name = n)
      }

    implicit def casePartialCommandFieldCommandFieldName:
      Case.Aux[PartialCommandField, CommandFieldName, CommandField] =

      at[PartialCommandField, CommandFieldName] {
        case (PartialCommandField(_, description), n: CommandFieldName) =>
          CommandField(n, description)
      }

    implicit def caseCommandFieldDescription:
      Case.Aux[CommandField, Option[Description], CommandField] =

      at[CommandField, Option[Description]] {
        case (field, d) =>
          field.copy(description = d)
      }

    implicit def casePartialCommandFieldDescription:
      Case.Aux[PartialCommandField, Option[Description], PartialCommandField] =

      at[PartialCommandField, Option[Description]] {
        case (field, d) =>
          field.copy(description = d)
      }
  }
}