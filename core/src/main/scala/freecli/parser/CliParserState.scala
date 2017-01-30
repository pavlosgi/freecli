package freecli
package parser

case class CliParserState(args: Seq[CliArgument], failMessage: Option[String])