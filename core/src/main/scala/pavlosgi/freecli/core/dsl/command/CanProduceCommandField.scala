package pavlosgi.freecli.core.dsl.command

import shapeless._
import shapeless.ops.hlist.LeftFolder

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.command.{CommandField, CommandFieldName}

trait CanProduceCommandField[H <: HList] {
  def apply(list: H): CommandField
}

object CanProduceCommandField {
  implicit def canProduceCommandField[H <: HList](
    implicit ev: LeftFolder.Aux[H, PartialCommandField, produceCommandField.type, CommandField]) =

  new CanProduceCommandField[H] {
    def apply(list: H): CommandField = {
      ev(list, PartialCommandField(None, None))
    }
  }
}

object produceCommandField extends Poly2 {
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