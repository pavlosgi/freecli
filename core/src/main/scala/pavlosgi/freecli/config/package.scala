package pavlosgi.freecli

import cats.data.{NonEmptyList, Validated, _}
import cats.syntax.all._

import pavlosgi.freecli.argument.interpreters.{help => AI}
import pavlosgi.freecli.config.dsl._
import pavlosgi.freecli.config.interpreters.help._
import pavlosgi.freecli.config.interpreters.parser._
import pavlosgi.freecli.core._

package object config
  extends Ops
  with ConfigDslImplicits {

  type ConfigDsl[A] = dsl.ConfigDsl[A]

  def parseConfig[A](
    args: Seq[String])
   (dsl: ConfigDsl[A]):
    ValidatedNel[ConfigParsingError, A] = {

    val (outArgs, res) =
      ResultT.run(CommandLineArguments.fromArgs(args))(
        dsl.foldMap(configParserInterpreter))

    outArgs.unmarked match {
      case Nil => res.toValidated
      case u =>
        val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(AdditionalArgumentsFound(u), ers))
    }
  }

  def configHelp[A](dsl: ConfigDsl[A]): String = {
    val result = dsl.analyze(configHelpInterpreter)
    val argsOneLine = result.arguments.map(AI.HelpState.oneline)

    s"""
       |${"Usage".bold.underline}
       |
       |  Program [options] ${argsOneLine.getOrElse("")}
       |
       |${HelpState.display(4, result)}
       |
       |""".stripMargin
  }

  def parseConfigOrHelp[A](args: Seq[String])(dsl: ConfigDsl[A]): A = {
    parseConfig(args)(dsl) match {
      case Validated.Valid(r) => r
      case Validated.Invalid(e) =>
        println(
          s"""${Error.displayErrors(e)}
             |${configHelp(dsl)}
             |""".stripMargin)

        sys.exit(1)
    }
  }
}
