package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config._

trait CanProduceField[H <: HList] {
  type Out <: HList

  def apply(list: H): (Field, Out)
}

object CanProduceField {
  type FieldTypes =
    FieldName :: FieldAbbreviation :: Description :: HNil

  case class PartialField(
    name: Option[FieldName],
    abbreviation: Option[FieldAbbreviation],
    description: Option[Description])

  type Aux[H <: HList, Out_ <: HList] = CanProduceField[H] {
    type Out = Out_
  }

  implicit def canProduceField[H <: HList, Out0 <: HList, Out1 <: HList](
    implicit ev: Intersection.Aux[H, FieldTypes, Out0],
    ev2: LeftFolder.Aux[Out0, PartialField, aggregate.type, Field],
    ev3: Diff.Aux[H, Out0, Out1]) = {

    new CanProduceField[H] {
      override type Out = Out1

      def apply(list: H): (Field, Out) = {
        val inters = ev.apply(list)
        val field = inters.foldLeft(PartialField(None, None, None))(aggregate)
        val remaining = ev3.apply(list)

        field -> remaining
      }
    }
  }

  object aggregate extends Poly2 {
    implicit def caseFieldFieldName:
      Case.Aux[Field, FieldName, Field] =

      at[Field, FieldName] {
        case (field, n: FieldName) =>
          Field.withFieldName(field, n)
      }

    implicit def casePartialFieldFieldName:
      Case.Aux[PartialField, FieldName, Field] =

      at[PartialField, FieldName] {
        case (PartialField(name, _, description), n: FieldName) =>
          FieldNameOnly(n, description)
      }

    implicit def caseFieldFieldAbbreviation:
      Case.Aux[Field, FieldAbbreviation, Field] =

      at[Field, FieldAbbreviation] {
        case (field, a: FieldAbbreviation) =>
          Field.withFieldAbbreviation(field, a)
      }

    implicit def casePartialFieldFieldAbbreviation:
      Case.Aux[PartialField, FieldAbbreviation, Field] =

      at[PartialField, FieldAbbreviation] {
        case (PartialField(_, _, description), a: FieldAbbreviation) =>
          FieldAbbreviationOnly(a, description)
      }

    implicit def caseFieldDescription:
      Case.Aux[Field, Description, Field] =

      at[Field, Description] {
        case (field, d) =>
          Field.withOptionalDescription(field, Some(d))
      }

    implicit def casePartialFieldDescription:
      Case.Aux[PartialField, Description, PartialField] =

      at[PartialField, Description] {
        case (field, d) =>
          field.copy(description = Some(d))
      }
  }
}