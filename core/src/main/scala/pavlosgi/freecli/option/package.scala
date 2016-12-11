package pavlosgi.freecli

import cats.data.{NonEmptyList, Validated, _}
import cats.syntax.all._

import pavlosgi.freecli.core._
import pavlosgi.freecli.option.dsl._
import pavlosgi.freecli.option.interpreters.help.{HelpState, optionHelpInterpreter}
import pavlosgi.freecli.option.interpreters.parser._

package object option
  extends OptionDslImplicits
  with Ops {

  type OptionDsl[A] = dsl.OptionDsl[A]

  def parseOptions[A](
    args: Seq[String])
   (dsl: OptionDsl[A]):
    ValidatedNel[OptionParsingError, A] = {

    val (outArgs, res) =
      ResultT.run(CommandLineArguments.fromArgs(args))(
        dsl.foldMap(optionParserInterpreter))

    outArgs.unmarked match {
      case Nil => res.toValidated
      case u =>
        val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(AdditionalArgumentsFound(u), ers))
    }
  }

  def optionsHelp[A](dsl: OptionDsl[A]): String = {
    val result = dsl.analyze(optionHelpInterpreter)

    s"""
       |${"Usage".bold.underline}
       |
       |  Program [options]
       |
       |${HelpState.display(4, result)}
       |
       |""".stripMargin
  }

  def parseOptionsOrHelp[A](args: Seq[String])(dsl: OptionDsl[A]): A = {
    parseOptions(args)(dsl) match {
      case Validated.Valid(r) => r
      case Validated.Invalid(e) =>
        println(
          s"""${Error.displayErrors(e)}
             |${optionsHelp(dsl)}
             |""".stripMargin)

        sys.exit(1)
    }
  }
}
