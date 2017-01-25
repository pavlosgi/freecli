package pavlosgi.freecli.config

import pavlosgi.freecli.argument.{dsl => AD}
import pavlosgi.freecli.config.api.{Action, ArgumentAction, OptionAction}
import pavlosgi.freecli.config.parser.ConfigParsingErrors
import pavlosgi.freecli.core.api.{Description, StringDecoder}
import pavlosgi.freecli.option.{ops => Op}
import pavlosgi.freecli.option.{dsl => OD}
import pavlosgi.freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends parser.ParserOps with help.HelpOps with AD.Ops {
  def flag(implicit ev: StringDecoder[Boolean]) = Op.flag(ev)
  def opt[T](implicit ev: StringDecoder[T]) = Op.opt(ev)
  def sub[T](description: Description) = Op.sub[T](description)
  def subT(description: Description) = Op.subT(description)
  def req = Op.req
  def or[T](default: T) = Op.or(default)
  def value(v: String) = Op.value(v)
  def version = Op.version
  def help = Op.help

  object O extends OD.Ops

  def parseConfigOrFail[A](args: Seq[String])(dsl: ConfigDsl[A]): A = {
    CliParser.runOrFail[Action, ConfigParsingErrors, A](
      args,
      configHelp(dsl),
      { case a@ArgumentAction(_) => a.run()
        case o@OptionAction(_) => o.run(configHelp(dsl))
      })(
      parseConfig(dsl))
  }
}