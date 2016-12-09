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

    val (outArgs, res) =
      ResultT.run(Arguments.fromStrings(args))(dsl.foldMap(commandParserInterpreter)(alternativeResultInstance))

    outArgs.unmarked match {
      case Nil => res.toValidated
      case u =>
        val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(
            NonEmptyList(AdditionalArgumentsFound(u.map(_.arg)), ers))
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
        println(
          s"""${Error.displayErrors(e)}
             |${commandHelp(dsl)}
             |""".stripMargin)

        sys.exit(1)
    }
  }
}
