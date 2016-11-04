package pavlosgi.freecli.core.interpreters.config.parser

sealed trait FieldValueOccurrence {
  def asBase: FieldValueOccurrence = this
}

case object FieldOnlyOccurrence extends FieldValueOccurrence
case class FieldAndValueOccurrence(value: String) extends FieldValueOccurrence

