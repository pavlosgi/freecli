package pavlosgi.freecli.core.dsl.command

import pavlosgi.freecli.core.api.command.{CommandFieldName, Description}

case class PartialCommandField(
  name: Option[CommandFieldName],
  description: Option[Description])
