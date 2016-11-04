package pavlosgi.freecli.core.dsl.config

import shapeless._
import shapeless.ops.hlist.{Diff, Intersection, LeftFolder}

import pavlosgi.freecli.core.api.config._

private[dsl] case class PartialField(
  name: Option[FieldName],
  abbreviation: Option[FieldAbbreviation],
  description: Option[Description])

private[dsl] object FieldBuilder {
  type FieldTypes =
    FieldName :: FieldAbbreviation :: Option[Description] :: HNil

  abstract class CanProduceField[H <: HList] {
    type IOut <: HList
    type DOut <: HList

    val intersection: Intersection.Aux[H, FieldTypes, IOut]
    val folder: LeftFolder.Aux[IOut, PartialField, aggregate.type, Field]
    val diff: Diff.Aux[H, IOut, DOut]

    def apply(list: H): (Field, DOut) = {
      val inters = intersection.apply(list)
      val field = getField(inters)(folder)
      val remaining = diff.apply(list)

      field -> remaining
    }
  }

  object CanProduceField {
    type Aux[H <: HList, Out <: HList] = CanProduceField[H] {
      type DOut = Out
    }

    implicit def canProduceField[H <: HList, Out0 <: HList, Out1 <: HList](
      implicit ev: Intersection.Aux[H, FieldTypes, Out0],
      ev2: LeftFolder.Aux[Out0, PartialField, aggregate.type, Field],
      ev3: Diff.Aux[H, Out0, Out1]) = {

      new CanProduceField[H] {
        override type IOut = Out0
        override type DOut = Out1
        override val intersection = ev
        override val folder = ev2
        override val diff = ev3
      }

    }
  }

  def getField[E <: HList](
    events: E)
   (implicit ev: LeftFolder.Aux[E, PartialField, aggregate.type, Field]):
    ev.Out = {

    events.foldLeft(PartialField(None, None, None))(aggregate)
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
      Case.Aux[Field, Option[Description], Field] =

      at[Field, Option[Description]] {
        case (field, d) =>
          Field.withOptionalDescription(field, d)
      }

    implicit def casePartialFieldDescription:
      Case.Aux[PartialField, Option[Description], PartialField] =

      at[PartialField, Option[Description]] {
        case (field, d) =>
          field.copy(description = d)
      }
  }
}