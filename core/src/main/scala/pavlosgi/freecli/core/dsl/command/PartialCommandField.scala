package pavlosgi.freecli.core.dsl.command

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.command.CommandFieldName

case class PartialCommandField(
  name: Option[CommandFieldName],
  description: Option[Description])
