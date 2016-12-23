package pavlosgi.freecli

import cats.data.Validated

import pavlosgi.freecli.config.dsl._
import pavlosgi.freecli.config.parser._
import pavlosgi.freecli.config.{parser => P}
import pavlosgi.freecli.config.{help => H}
import pavlosgi.freecli.parser.{CliFailure, CliParser}

package object config
  extends Ops
  with ConfigDslImplicits {

  type ConfigDsl[A] = dsl.ConfigDsl[A]

  def parseConfig[A](
    args: Seq[String])
   (dsl: ConfigDsl[A]):
    Validated[CliFailure[ConfigParsingError], A] = {

    P.parseConfig(args)(dsl)
  }

  def configHelp[A](dsl: ConfigDsl[A]): String = {
    H.configHelp(dsl)
  }

  def parseConfigOrHelp[A](args: Seq[String])(dsl: ConfigDsl[A]): A = {
    CliParser.runOrFail(args, _ => configHelp(dsl))(dsl.foldMap(ConfigParserInterpreter))
  }
}
