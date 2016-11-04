package pavlosgi.freecli.core.interpreters.command.parser

import pavlosgi.freecli.core.api.command.CommandFieldName
import pavlosgi.freecli.core.interpreters.Arguments

case class ParserState(args: Arguments, seen: Seq[CommandFieldName])