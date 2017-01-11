package pavlosgi.freecli.argument.parser

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._

import pavlosgi.freecli.argument.dsl.ArgumentDsl
import pavlosgi.freecli.parser.{CliFailure, CliParser}

trait Ops {
  def parseArguments[A](
    args: Seq[String])
   (dsl: ArgumentDsl[A]):
    Validated[CliFailure[ArgumentParsingError], A] = {

    val (arguments, res) =
      CliParser.run(args)(dsl.foldMap(ArgumentParserInterpreter))

    arguments.usable match {
      case Nil => res.toValidated
      case u =>
        val error = CliFailure.errors[ArgumentParsingError](
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
