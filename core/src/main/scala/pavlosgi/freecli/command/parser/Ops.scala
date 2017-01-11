package pavlosgi.freecli.command.parser

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._

import pavlosgi.freecli.command.dsl.CommandDsl
import pavlosgi.freecli.parser.{CliFailure, CliParser}

trait Ops {
  def parseCommand[A](
    args: Seq[String])
   (dsl: CommandDsl[A]):
    Validated[CliFailure[CommandParsingError], A] = {

    val (arguments, res) =
      CliParser.run(args)(
        dsl.foldMap(CommandParserInterpreter))

    arguments.usable match {
      case Nil => res.toValidated
      case u =>
        val error = CliFailure.errors[CommandParsingError](
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
