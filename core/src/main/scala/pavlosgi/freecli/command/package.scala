package pavlosgi.freecli

import cats.data.Validated

import pavlosgi.freecli.command.dsl._
import pavlosgi.freecli.command.parser.CommandParserInterpreter
import pavlosgi.freecli.command.{parser => P}
import pavlosgi.freecli.command.{help => H}
import pavlosgi.freecli.parser.{CliFailure, CliParser}

package object command
  extends Ops
  with CommandDslImplicits {

  type CommandDsl[A] = dsl.CommandDsl[A]

  def parseCommand[A](
    args: Seq[String])
   (dsl: CommandDsl[A]):
    Validated[CliFailure[P.CommandParsingError], A] = {

    P.parseCommand(args)(dsl)
  }

  def commandHelp[A](dsl: CommandDsl[A]): String = H.commandHelp(dsl)

  def parseCommandOrHelp[A](args: Seq[String])(dsl: CommandDsl[A]): A = {
    CliParser.runOrFail(args, H.commandHelpWithPath(dsl))(dsl.foldMap(CommandParserInterpreter))
  }
}
