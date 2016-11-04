package pavlosgi.freecli.core.interpreters.parser.command

import pavlosgi.freecli.core.api.command.CommandFieldName
import pavlosgi.freecli.core.interpreters.parser.Arguments

case class ParserState(args: Arguments, seen: Seq[CommandFieldName])