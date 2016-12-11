package pavlosgi.freecli

import cats.syntax.all._
import cats.data.{NonEmptyList, Validated}

import pavlosgi.freecli.argument.dsl._
import pavlosgi.freecli.argument.interpreters.help.{HelpState, argumentHelpInterpreter}
import pavlosgi.freecli.argument.interpreters.parser._
import pavlosgi.freecli.core._
import pavlosgi.freecli.core.parsing.ParsingFailure

package object argument
  extends Ops
  with ArgumentDslImplicits {

  type ArgumentDsl[A] = dsl.ArgumentDsl[A]

  def parseConfig[A](
    args: Seq[String])
   (dsl: ArgumentDsl[A]):
    Validated[ParsingFailure[ArgumentParsingError], A] = {

    val (outArgs, res) =
      ResultT.run(CommandLineArguments.fromArgs(args))(
        dsl.foldMap(argumentParserInterpreter))

    outArgs.unmarked match {
      case Nil => res.toValidated.leftMap(ers => ParsingFailure(outArgs, ers))
      case u =>
        val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(ParsingFailure(
            outArgs,
            NonEmptyList(AdditionalArgumentsFound(u), ers)))
    }
  }

  def argumentsHelp[A](dsl: ArgumentDsl[A]): String = {
    val result = dsl.analyze(argumentHelpInterpreter)
    val argsOneLine = HelpState.oneline(result)

    s"""${"Usage".bold.underline}
       |
       |  Program $argsOneLine
       |
       |${HelpState.display(4, result)}
       |
       |""".stripMargin
  }

  def parseArgumentsOrHelp[A](args: Seq[String])(dsl: ArgumentDsl[A]): A = {
    parsing.getOrReportAndExit(parseConfig(args)(dsl), argumentsHelp(dsl))
  }

}
