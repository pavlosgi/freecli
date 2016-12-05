package pavlosgi.freecli.core.dsl

import cats.free.FreeApplicative

import pavlosgi.freecli.core.api.{Description, StringDecoder}
import pavlosgi.freecli.core.api.options.Algebra

package object options extends OptionDslImplicits with TransformationOps {
  type OptionsDsl[A] = FreeApplicative[Algebra, A]

  import pavlosgi.freecli.core.dsl.options.OptDslBuilder.DefaultValue

  def string = opt[String]
  def int = opt[Int]
  def boolean = opt[Boolean]
  def flag(implicit ev: StringDecoder[Boolean]) = FlagDslBuilder.flag
  def req[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T] -~ required
  def opt[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T]
  def sub(description: String) =
    SubDslBuilder.sub(Description(description))

  def required = OptDslBuilder.required
  def or[T](default: T): DefaultValue[T] = DefaultValue(default)
  def des(description: String): Description = Description(description)
}