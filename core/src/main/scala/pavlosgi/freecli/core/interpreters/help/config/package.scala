package pavlosgi.freecli.core.interpreters.help

import cats.Monoid
import cats.~>

import pavlosgi.freecli.core.api.config._
import pavlosgi.freecli.core.dsl.config.ConfigDsl

package object config {
  type Result[A] = HelpState

  def configHelp[A](dsl: ConfigDsl[A]): String = {
    val result = dsl.analyze(configAlgebraHelp)
    val argsOneLine = result.arguments.map(ArgumentsHelp.oneline)

    s"""
       |${"Usage".bold.underline}
       |
       |  Program [options] ${argsOneLine.getOrElse("")}
       |
       |${HelpState.display(4, result)}
       |
       |""".stripMargin
  }

  implicit object configAlgebraHelp extends (Algebra ~> Result) {
    def apply[A](fa: Algebra[A]): HelpState = {
      fa match {
        case Arg(details, _, _) =>
          Monoid[HelpState].empty.addArgument(details)

        case RequiredOpt(field, _, g, default) =>
          Monoid[HelpState].empty
            .addOption(OptionFieldHelp(field, default.map(g.toString)))

        case Opt(field, _, _) =>
          Monoid[HelpState].empty
            .addOption(OptionFieldHelp(field))

        case Flag(field, _) =>
          Monoid[HelpState].empty
            .addOption(OptionFieldHelp(field))

        case Sub(description, dsl) =>
          Monoid[HelpState].empty
            .addOption(SubHelp(description, dsl.analyze(configAlgebraHelp)))
      }
    }
  }
}