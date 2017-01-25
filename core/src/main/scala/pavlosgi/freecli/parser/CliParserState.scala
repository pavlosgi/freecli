package pavlosgi.freecli.parser

case class CliParserState(args: Seq[CliArgument], failMessage: Option[String])