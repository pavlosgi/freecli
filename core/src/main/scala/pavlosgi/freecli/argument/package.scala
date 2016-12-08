package pavlosgi.freecli

import cats.syntax.all._
import cats.data.{NonEmptyList, Validated, _}

import pavlosgi.freecli.argument.dsl._
import pavlosgi.freecli.argument.interpreters.help.{HelpState, argumentHelpInterpreter}
import pavlosgi.freecli.argument.interpreters.parser.{AdditionalArgumentsFound, ArgumentParsingError, argumentParserInterpreter}
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

    ResultTS.run(Arguments(args))(dsl.foldMap(argumentParserInterpreter)) match {
        case (Arguments(Nil), res) => res.toValidated
        case (Arguments(argsLeft), res) =>
          val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(AdditionalArgumentsFound(argsLeft), ers))
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
        println(e.toList.mkString(", "))
        println(argumentsHelp(dsl))
        sys.exit(1)
    }
  }

}
