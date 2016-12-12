package pavlosgi.freecli

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._

import pavlosgi.freecli.core._
import pavlosgi.freecli.core.parsing.ParsingFailure
import pavlosgi.freecli.option.dsl._
import pavlosgi.freecli.option.interpreters.help.OptionHelpInterpreter
import pavlosgi.freecli.option.interpreters.parser._

package object option
  extends OptionDslImplicits
  with Ops {

  type OptionDsl[A] = dsl.OptionDsl[A]

  def parseOptions[A](
    args: Seq[String])
   (dsl: OptionDsl[A]):
    Validated[ParsingFailure[OptionParsingError], A] = {

    val (outArgs, res) =
      ResultT.run(CommandLineArguments.fromArgs(args))(
        dsl.foldMap(optionParserInterpreter))

    outArgs.unmarked match {
      case Nil => res.toValidated.leftMap(ers => ParsingFailure(outArgs, ers))
      case u =>
        val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(ParsingFailure(
            outArgs,
            NonEmptyList(AdditionalArgumentsFound(u), ers)))
    }
  }

  def optionsHelp[A](dsl: OptionDsl[A]): String = {
    val result = dsl.analyze(OptionHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |  Program [options]
       |
       |${result.result.display(4)}
       |""".stripMargin
  }

  def parseOptionsOrHelp[A](args: Seq[String])(dsl: OptionDsl[A]): A = {
    parsing.getOrReportAndExit(parseOptions(args)(dsl), optionsHelp(dsl))
  }
}
