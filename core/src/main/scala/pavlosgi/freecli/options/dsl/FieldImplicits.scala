package pavlosgi.freecli.options.dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.{CanProduce, Description}
import pavlosgi.freecli.core.Description
import pavlosgi.freecli.options.api._

trait FieldImplicits {
  type FieldTypes = FieldName :: FieldAbbreviation :: Description :: HNil

  case class PartialField(
    name: Option[FieldName],
    abbreviation: Option[FieldAbbreviation],
    description: Option[Description])

  implicit def canProduceField[H <: HList, Out0 <: HList, Rem <: HList](
    implicit ev: Intersection.Aux[H, FieldTypes, Out0],
    ev2: LeftFolder.Aux[Out0, PartialField, fieldBuilder.type, Field],
    ev3: Diff.Aux[H, Out0, Rem]) = {

    new CanProduce[H] {
      type Out = (Field, Rem)

      def apply(list: H): Out = {
        val inters = ev.apply(list)
        val field = inters.foldLeft(PartialField(None, None, None))(fieldBuilder)
        val remaining = ev3.apply(list)

        field -> remaining
      }
    }
  }

  object fieldBuilder extends Poly2 {
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