package pavlosgi.freecli

import cats.data.Validated

import pavlosgi.freecli.option.dsl._
import pavlosgi.freecli.option.{help => H}
import pavlosgi.freecli.option.parser.{OptionParserInterpreter, OptionParsingError}
import pavlosgi.freecli.option.{parser => P}
import pavlosgi.freecli.parser.{CliFailure, CliParser}

package object option
  extends OptionDslImplicits
  with Ops {

  type OptionDsl[A] = dsl.OptionDsl[A]

  def parseOptions[A](
    args: Seq[String])
   (dsl: OptionDsl[A]):
    Validated[CliFailure[OptionParsingError], A] = {

    P.parseOptions(args)(dsl)
  }

  def optionsHelp[A](dsl: OptionDsl[A]): String = {
    H.optionsHelp(dsl)
  }

  def parseOptionsOrHelp[A](args: Seq[String])(dsl: OptionDsl[A]): A = {
    CliParser.runOrFail(args, _ => optionsHelp(dsl))(dsl.foldMap(OptionParserInterpreter))
  }
}
