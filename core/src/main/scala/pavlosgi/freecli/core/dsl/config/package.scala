package pavlosgi.freecli.core.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.Tupler

import pavlosgi.freecli.core.api.config.{Description, StringDecoder}

package object config {
  def config[T]: ConfigDsl.Apply[T] = new ConfigDsl.Apply[T]
  def config[T <: HList, Tup](
    c: ConfigDsl[T])
   (implicit ev: Tupler.Aux[T, Tup]):
    ConfigDsl[Tup] = c.map(ev.apply)

  def flag(implicit ev: StringDecoder[Boolean]) = FlagDsl.flag

  def boolean = FlagDsl.flag

  def required = new Required {}

  def opt[T](implicit ev: StringDecoder[T]) = OptDsl.opt[T]

  def optString = opt[String]

  def optInt = opt[Int]

  def req[T](implicit ev: StringDecoder[T]) = OptDsl.opt[T] -~ required

  def string = req[String]

  def int = req[Int]

  def sub[T] = SubDsl.sub[T]

  def sub[T](description: String) =
    SubDsl.sub[Description :: HNil, T](Description(description) :: HNil)

  def des(description: String): Description = Description(description)
  def or[T](default: T): DefaultValue[T] = DefaultValue(default)
}
