package pavlosgi.freecli

import cats.syntax.all._
import cats.data.{NonEmptyList, Validated}

import pavlosgi.freecli.command.dsl._
import pavlosgi.freecli.command.interpreters.help._
import pavlosgi.freecli.command.interpreters.parser._
import pavlosgi.freecli.core.formatting._
import pavlosgi.freecli.parser.{CliParser, ParsingFailure}

package object command
  extends Ops
  with CommandDslImplicits {

  type CommandDsl[A] = dsl.CommandDsl[A]

  def parseCommand[A](
    args: Seq[String])
   (dsl: CommandDsl[A]):
    Validated[ParsingFailure[CommandParsingError], A] = {

    val (outArgs, res) =
      CliParser.run(args)(
        dsl.foldMap(commandParserInterpreter)(alternativeResultInstance))

    outArgs.usable match {
      case Nil => res.toValidated.leftMap(ers => ParsingFailure(outArgs, ers))
      case u =>
        val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(ParsingFailure(
            outArgs, NonEmptyList(AdditionalArgumentsFound(u.map(_.name)), ers)))
    }
  }

  def commandHelp[A](dsl: CommandDsl[A]): String = {
    val commands = dsl.analyze(commandHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |${commands.result.display(2)}
       |""".stripMargin
  }

  def parseCommandOrHelp[A](args: Seq[String])(dsl: CommandDsl[A]): A = {
    parser.getOrReportAndExit(parseCommand(args)(dsl), commandHelp(dsl))
  }
}
