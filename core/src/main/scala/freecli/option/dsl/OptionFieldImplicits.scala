package freecli
package option
package dsl

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import core.api.{CanProduce, Description}
import option.api._

trait OptionFieldImplicits {
  type OptionFieldTypes =
    OptionFieldName :: OptionFieldAbbreviation :: Description :: HNil

  case class PartialOptionField(
    name: Option[OptionFieldName],
    abbreviation: Option[OptionFieldAbbreviation],
    description: Option[Description])

  implicit def canProduceOptionField[H <: HList, Out0 <: HList, Rem <: HList](
    implicit ev: Intersection.Aux[H, OptionFieldTypes, Out0],
    ev2: LeftFolder.Aux[Out0, PartialOptionField, fieldBuilder.type, OptionField],
    ev3: Diff.Aux[H, Out0, Rem]) = {

    new CanProduce[H] {
      type Out = (OptionField, Rem)

      def apply(list: H): Out = {
        val inters = ev.apply(list)
        val field = inters.foldLeft(PartialOptionField(None, None, None))(fieldBuilder)
        val remaining = ev3.apply(list)

        field -> remaining
      }
    }
  }

  object fieldBuilder extends Poly2 {
    implicit def caseOptionFieldFieldName:
      Case.Aux[OptionField, OptionFieldName, OptionField] =

      at[OptionField, OptionFieldName] {
        case (field, n: OptionFieldName) =>
          OptionField.withFieldName(field, n)
      }

    implicit def casePartialOptionFieldFieldName:
      Case.Aux[PartialOptionField, OptionFieldName, OptionField] =

      at[PartialOptionField, OptionFieldName] {
        case (PartialOptionField(name, _, description), n: OptionFieldName) =>
          OptionFieldNameOnly(n, description)
      }

    implicit def caseOptionFieldOptionFieldAbbreviation:
      Case.Aux[OptionField, OptionFieldAbbreviation, OptionField] =

      at[OptionField, OptionFieldAbbreviation] {
        case (field, a: OptionFieldAbbreviation) =>
          OptionField.withFieldAbbreviation(field, a)
      }

    implicit def casePartialOptionFieldOptionFieldAbbreviation:
      Case.Aux[PartialOptionField, OptionFieldAbbreviation, OptionField] =

      at[PartialOptionField, OptionFieldAbbreviation] {
        case (PartialOptionField(_, _, description), a: OptionFieldAbbreviation) =>
          OptionFieldAbbreviationOnly(a, description)
      }

    implicit def caseOptionFieldDescription:
      Case.Aux[OptionField, Description, OptionField] =

      at[OptionField, Description] {
        case (field, d) =>
          OptionField.withOptionalDescription(field, Some(d))
      }

    implicit def casePartialOptionFieldDescription:
      Case.Aux[PartialOptionField, Description, PartialOptionField] =

      at[PartialOptionField, Description] {
        case (field, d) =>
          field.copy(description = Some(d))
      }
  }
}