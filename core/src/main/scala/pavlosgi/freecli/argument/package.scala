package pavlosgi.freecli

import cats.data.Validated

import pavlosgi.freecli.argument.dsl._
import pavlosgi.freecli.argument.parser.ArgumentParserInterpreter
import pavlosgi.freecli.argument.{help => H}
import pavlosgi.freecli.argument.{parser => P}
import pavlosgi.freecli.parser.{CliFailure, CliParser}

package object argument
  extends Ops
  with ArgumentDslImplicits {

  type ArgumentDsl[A] = dsl.ArgumentDsl[A]

  def parseArguments[A](
    args: Seq[String])
   (dsl: ArgumentDsl[A]):
    Validated[CliFailure[P.ArgumentParsingError], A] = {

    P.parseArguments(args)(dsl)
  }

  def argumentsHelp[A](dsl: ArgumentDsl[A]): String = {
    H.argumentsHelp(dsl)
  }

  def parseArgumentsOrHelp[A](args: Seq[String])(dsl: ArgumentDsl[A]): A = {
    CliParser.runOrFail(args, _ => argumentsHelp(dsl))(dsl.foldMap(ArgumentParserInterpreter))
  }

}
