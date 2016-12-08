package pavlosgi.freecli

import cats.syntax.all._
import cats.data.{NonEmptyList, Validated, _}

import pavlosgi.freecli.command.dsl._
import pavlosgi.freecli.command.interpreters.help._
import pavlosgi.freecli.command.interpreters.parser._
import pavlosgi.freecli.core._

package object command
  extends Ops
  with CommandDslBuilderImplicits
  with CommandDslImplicits
  with CommandFieldImplicits {

  type CommandDsl[A] = dsl.CommandDsl[A]

  def parseCommand[A](
    args: Seq[String])
   (dsl: CommandDsl[A]):
    ValidatedNel[CommandParsingError, A] = {

    val resultT: ResultT[A] =
      dsl.foldMap(commandParserInterpreter)(alternativeResultInstance)

    ResultTS.run[CommandParsingError, Arguments, A](Arguments(args))(resultT) match {
      case (Arguments(Nil), res) => res.toValidated
      case (Arguments(argsLeft), res) =>
        val ers = res.fold(_.toList, _ => List.empty)
        Validated.invalid(
          NonEmptyList(AdditionalArgumentsFound(argsLeft), ers))
    }
  }

  def commandHelp[A](dsl: CommandDsl[A]): String = {
    val result = dsl.analyze(commandHelpInterpreter)

    s"""
     |${"Usage".bold.underline}
     |
     |${HelpState.display(2, result)}
     |
     |""".stripMargin
  }

  def parseCommandOrHelp[A](args: Seq[String])(dsl: CommandDsl[A]): A = {
    parseCommand(args)(dsl) match {
      case Validated.Valid(r) => r
      case Validated.Invalid(e) =>
        println(e.toList.mkString(", "))
        println(commandHelp(dsl))
        sys.exit(1)
    }
  }
}
