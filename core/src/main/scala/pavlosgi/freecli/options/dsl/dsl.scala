package pavlosgi.freecli.options

import cats.free.FreeApplicative

import pavlosgi.freecli.core.{Description, StringDecoder, Grouping}
import pavlosgi.freecli.options.api.Algebra
import pavlosgi.freecli.options.dsl.OptDslBuilder.DefaultValue

package object dsl extends OptionDslImplicits with Grouping {
  type OptionsDsl[A] = FreeApplicative[Algebra, A]

  def string = opt[String]
  def int = opt[Int]
  def boolean = opt[Boolean]
  def flag(implicit ev: StringDecoder[Boolean]) = FlagDslBuilder.flag
  def req[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T] -~ required
  def opt[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T]
  def sub[T](description: String) =
    SubDslBuilder.sub[T](Description(description))

  def subT(description: String) =
    SubDslBuilder.subT(Description(description))

  def required = OptDslBuilder.required
  def or[T](default: T): DefaultValue[T] = DefaultValue(default)
  def des(description: String): Description = Description(description)
}

