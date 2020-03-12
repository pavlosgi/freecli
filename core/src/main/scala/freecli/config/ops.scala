package freecli
package config

import api.{Action, ArgumentAction, OptionAction}
import argument.{dsl => AD}
import parser.ConfigParsingErrors
import core.api.{Description, StringDecoder}
import freecli.parser.CliParser
import option.{ops => Op}
import option.{dsl => OD}

object ops extends AllOps
trait AllOps extends parser.ParserOps with help.HelpOps with AD.Ops {
  def flag(implicit ev: StringDecoder[Boolean]) = Op.flag(ev)
  def opt[T](implicit ev: StringDecoder[T])     = Op.opt(ev)
  def sub[T](description: Description)          = Op.sub[T](description)
  def subT(description: Description)            = Op.subT(description)
  def req                                       = Op.req
  def or[T](default: T)                         = Op.or(default)
  def value(v: String)                          = Op.value(v)
  def version                                   = Op.version
  def help                                      = Op.help

  object O extends OD.Ops

  def runConfigOrFail[A](dsl: ConfigDsl[A])(args: Array[String]): A =
    CliParser.runOrFail[Action, ConfigParsingErrors, A](
      args.toIndexedSeq,
      Some(configHelp(dsl)), {
        case a @ ArgumentAction(_) => a.run()
        case o @ OptionAction(_)   => o.run(configHelp(dsl))
      }
    )(parseConfig(dsl))
}
