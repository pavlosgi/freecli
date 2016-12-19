package pavlosgi.freecli

import cats.data.{NonEmptyList, Validated}
import cats.syntax.all._

import pavlosgi.freecli.config.dsl._
import pavlosgi.freecli.config.interpreters.help._
import pavlosgi.freecli.config.interpreters.parser._
import pavlosgi.freecli.core.formatting._
import pavlosgi.freecli.parser.{CliParser, ParsingFailure}

package object config
  extends Ops
  with ConfigDslImplicits {

  type ConfigDsl[A] = dsl.ConfigDsl[A]

  def parseConfig[A](
    args: Seq[String])
   (dsl: ConfigDsl[A]):
    Validated[ParsingFailure[ConfigParsingError], A] = {

    val (outArgs, res) =
      CliParser.run(args)(dsl.foldMap(configParserInterpreter))

    outArgs.usable match {
      case Nil => res.toValidated.leftMap(ers => ParsingFailure(outArgs, ers))
      case u =>
        val ers = res.fold(_.toList, _ => List.empty)
          Validated.invalid(ParsingFailure(
            outArgs,
            NonEmptyList(AdditionalArgumentsFound(u.map(_.name)), ers)))
    }
  }

  def configHelp[A](dsl: ConfigDsl[A]): String = {
    val config = dsl.analyze(configHelpInterpreter)

    s"""${"Usage".bold.underline}
       |
       |  Program [options] ${config.oneline.display(0)}
       |
       |${config.result.display(4)}
       |""".stripMargin
  }

  def parseConfigOrHelp[A](args: Seq[String])(dsl: ConfigDsl[A]): A = {
    parser.getOrReportAndExit(parseConfig(args)(dsl), configHelp(dsl))
  }
}
