package pavlosgi.freecli

import cats.syntax.all._
import cats.data.{NonEmptyList, Validated, _}

import pavlosgi.freecli.argument.dsl._
import pavlosgi.freecli.argument.interpreters.help.{HelpState, argumentHelpInterpreter}
import pavlosgi.freecli.argument.interpreters.parser._
import pavlosgi.freecli.core._

package object argument
  extends Ops
  with MergerImplicits
  with ArgumentsDetailsImplicits
  with ArgumentDslImplicits {

  type ArgumentDsl[A] = dsl.ArgumentDsl[A]

  def parseConfig[A](
    args: Seq[String])
   (dsl: ArgumentDsl[A]):
    ValidatedNel[ArgumentParsingError, A] = {

    val (outArgs, res) =
      ResultT.run(Arguments.fromStrings(args))(dsl.foldMap(argumentParserInterpreter))

    outArgs.unmarked match {
      case Nil => res.toValidated
      case u =>
        val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(AdditionalArgumentsFound(u.map(_.arg)), ers))
    }
  }

  def argumentsHelp[A](dsl: ArgumentDsl[A]): String = {
    val result = dsl.analyze(argumentHelpInterpreter)
    val argsOneLine = HelpState.oneline(result)

    s"""
       |${"Usage".bold.underline}
       |
       |  Program $argsOneLine
       |
       |${HelpState.display(4, result)}
       |
       |""".stripMargin
  }

  def parseArgumentsOrHelp[A](args: Seq[String])(dsl: ArgumentDsl[A]): A = {
    parseConfig(args)(dsl) match {
      case Validated.Valid(r) => r
      case Validated.Invalid(e) =>
        println(
          s"""${Error.displayErrors(e)}
             |${argumentsHelp(dsl)}
             |""".stripMargin)

        sys.exit(1)
    }
  }

}
