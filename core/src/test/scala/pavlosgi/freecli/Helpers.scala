package pavlosgi.freecli

import pavlosgi.freecli.parser.{CliFailure, HelpRequested}

object Helpers {
  implicit class CliFailureOps[E](cliFailure: CliFailure[E]) {
    def errors = cliFailure.value match {
      case Left(HelpRequested) =>
        throw new IllegalArgumentException("Tried to access errors in failure but failure was HelpRequested")

      case Right(errors) => errors
    }
  }
}
