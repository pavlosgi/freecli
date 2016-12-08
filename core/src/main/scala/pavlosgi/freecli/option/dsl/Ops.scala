package pavlosgi.freecli.option.dsl

import pavlosgi.freecli.core.{Description, Grouping, StringDecoder}
import pavlosgi.freecli.option.dsl.OptDslBuilder.DefaultValue

trait Ops extends Grouping {
  def string = opt[String]
  def int = opt[Int]
  def boolean = opt[Boolean]
  def flag(implicit ev: StringDecoder[Boolean]) = FlagDslBuilder.flag
  def opt[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T]
  def sub[T](description: String) =
    SubDslBuilder.sub[T](Description(description))

  def subT(description: String) =
    SubDslBuilder.subT(Description(description))

  def req = OptDslBuilder.required
  def or[T](default: T): DefaultValue[T] = DefaultValue(default)
  def des(description: String): Description = Description(description)
}
