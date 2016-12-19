package pavlosgi.freecli

import cats.syntax.all._
import cats.data.{NonEmptyList, Validated}

import pavlosgi.freecli.argument.dsl._
import pavlosgi.freecli.argument.interpreters.help._
import pavlosgi.freecli.argument.interpreters.parser._
import pavlosgi.freecli.core.formatting._
import pavlosgi.freecli.parser.{CliParser, ParsingFailure}

package object argument
  extends Ops
  with ArgumentDslImplicits {

  type ArgumentDsl[A] = dsl.ArgumentDsl[A]

  def parseArguments[A](
    args: Seq[String])
   (dsl: ArgumentDsl[A]):
    Validated[ParsingFailure[ArgumentParsingError], A] = {

    val (outArgs, res) =
      CliParser.run(args)(dsl.foldMap(argumentParserInterpreter))

    outArgs.usable match {
      case Nil => res.toValidated.leftMap(ers => ParsingFailure(outArgs, ers))
      case u =>
        val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(ParsingFailure(
            outArgs,
            NonEmptyList(AdditionalArgumentsFound(u.map(_.name)), ers)))
    }
  }

  def argumentsHelp[A](dsl: ArgumentDsl[A]): String = {
    val arguments = dsl.analyze(ArgumentsHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |  Program ${arguments.oneline.display()}
       |
       |${arguments.result.display(4)}
       |""".stripMargin
  }

  def parseArgumentsOrHelp[A](args: Seq[String])(dsl: ArgumentDsl[A]): A = {
    parser.getOrReportAndExit(parseArguments(args)(dsl), argumentsHelp(dsl))
  }

}
