package freecli
package option
package dsl

import java.io.File

import core.api.{Description, ExistentFile, NewFile, StringDecoder}
import option.api.StringValue
import option.dsl.OptDslBuilder.DefaultValue

trait Ops {
  def string = opt[String]
  def int = opt[Int]
  def long = opt[Long]
  def double = opt[Double]
  def boolean = opt[Boolean]
  def file = opt[File]
  def existentFile = opt[ExistentFile]
  def newFile = opt[NewFile]
  def flag(implicit ev: StringDecoder[Boolean]) = FlagDslBuilder.flag
  def help = HelpDslBuilder.help
  def version = VersionDslBuilder.version
  def value(v: String) = StringValue(v)
  def opt[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T]
  def sub[T](description: Description) =
    SubDslBuilder.sub[T](description)

  def subT(description: Description) =
    SubDslBuilder.subT(description)

  def req = OptDslBuilder.required
  def or[T](default: T): DefaultValue[T] = DefaultValue(default)
}
