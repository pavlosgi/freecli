package pavlosgi.freecli.parser

import cats.data.NonEmptyList

case class ParsingFailure[E: Error](args: CliArguments, errors: NonEmptyList[E])