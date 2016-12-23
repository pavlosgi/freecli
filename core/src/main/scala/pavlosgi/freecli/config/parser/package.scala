package pavlosgi.freecli.config

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._

import pavlosgi.freecli.parser.{CliFailure, CliParser}

package object parser {
  type ParseResult[A] = CliParser[ConfigParsingError, A]

    def parseConfig[A](
    args: Seq[String])
   (dsl: ConfigDsl[A]):
    Validated[CliFailure[ConfigParsingError], A] = {

    val (arguments, res) =
      CliParser.run(args)(dsl.foldMap(ConfigParserInterpreter))

    arguments.usable match {
      case Nil => res.toValidated
      case u =>
        val error = CliFailure.errors[ConfigParsingError](
          NonEmptyList.of(AdditionalArgumentsFound(u.map(_.name))))

        res match {
          case Left(failure) =>
            Validated.invalid(failure.combine(error))

          case Right(_) =>
            Validated.invalid(error)
        }
    }
  }
}
