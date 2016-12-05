package pavlosgi.freecli.command.dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.command.api.{CommandField, CommandFieldName}
import pavlosgi.freecli.core.{CanProduce, Description}

trait CommandFieldImplicits {
  type CommandFieldTypes = Description :: CommandFieldName :: HNil

  case class PartialCommandField(
    name: Option[CommandFieldName],
    description: Option[Description])

  implicit def canProduceCommandField[T, H <: HList, Out0 <: HList, Rem <: HList](
    implicit ev: Intersection.Aux[H, CommandFieldTypes, Out0],
    ev2: LeftFolder.Aux[Out0, PartialCommandField, commandFieldBuilder.type, CommandField],
    ev3: Diff.Aux[H, Out0, Rem]) =

    new CanProduce[H] {
      type Out = (CommandField, Rem)

      def apply(list: H): Out = {
        val inters = ev.apply(list)
        val field = inters.foldLeft(PartialCommandField(None, None))(commandFieldBuilder)
        val remaining = ev3.apply(list)

        field -> remaining
      }
    }

    object commandFieldBuilder extends Poly2 {
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
        Case.Aux[CommandField, Description, CommandField] =

        at[CommandField, Description] {
          case (field, d) =>
            field.copy(description = Some(d))
        }

      implicit def casePartialCommandFieldDescription:
        Case.Aux[PartialCommandField, Description, PartialCommandField] =

        at[PartialCommandField, Description] {
          case (field, d) =>
            field.copy(description = Some(d))
        }
    }
}