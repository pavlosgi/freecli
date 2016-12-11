package pavlosgi.freecli.core.parsing

import cats.data.NonEmptyList

import pavlosgi.freecli.core.CommandLineArguments

case class ParsingFailure[E: Error](args: CommandLineArguments, errors: NonEmptyList[E])