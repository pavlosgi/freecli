package pavlosgi.freecli.config

import pavlosgi.freecli.argument.{ops => A}
import pavlosgi.freecli.config.{parser => P}
import pavlosgi.freecli.core.api.{Description, StringDecoder}
import pavlosgi.freecli.option.{ops => Op}
import pavlosgi.freecli.parser.CliParser

object ops extends AllOps
trait AllOps extends parser.Ops with help.Ops {
  def arg[T](implicit ev: StringDecoder[T]) = A.arg[T]

  def string = A.string
  def int = A.int
  def long = A.long
  def double = A.double
  def boolean = A.boolean
  def file = A.file
  def existentFile = A.existentFile
  def newFile = A.newFile
  def name(name: String) = A.name(name)

  def flag(implicit ev: StringDecoder[Boolean]) = Op.flag(ev)
  def opt[T](implicit ev: StringDecoder[T]) = Op.opt(ev)
  def sub[T](description: Description) = Op.sub[T](description)
  def subT(description: Description) = Op.subT(description)

  def req = Op.req
  def or[T](default: T) = Op.or(default)

  object O {
    def string = Op.string
    def int = Op.int
    def long = Op.long
    def double = Op.double
    def boolean = Op.boolean
    def file = Op.file
    def existentFile = Op.existentFile
    def newFile = Op.newFile
    def help = Op.help_
  }

  def parseConfigOrHelp[A](args: Seq[String])(dsl: ConfigDsl[A]): A = {
    CliParser.runOrFail(args, _ => configHelp(dsl))(dsl.foldMap(P.ConfigParserInterpreter))
  }
}