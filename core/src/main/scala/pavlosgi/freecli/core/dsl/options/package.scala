package pavlosgi.freecli.core.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.Tupler

import pavlosgi.freecli.core.api.options.{Description, StringDecoder}

package object options {
  def options[T]: OptionsDsl.Apply[T] = new OptionsDsl.Apply[T]
  def options[T <: HList, Tup](
    c: OptionsDsl[T])
   (implicit ev: Tupler.Aux[T, Tup]):
    OptionsDsl[Tup] = c.map(ev.apply)

  def flag(implicit ev: StringDecoder[Boolean]) = FlagDslBuilder.flag

  def boolean = FlagDslBuilder.flag

  def required = new Required {}

  def opt[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T]

  def optString = opt[String]

  def optInt = opt[Int]

  def req[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T] -~ required

  def string = req[String]

  def int = req[Int]

  def sub[T] = SubDslBuilder.sub[T]

  def sub[T](description: String) =
    SubDslBuilder.sub[Description :: HNil, T](Description(description) :: HNil)

  def des(description: String): Description = Description(description)
  def or[T](default: T): DefaultValue[T] = DefaultValue(default)
}
