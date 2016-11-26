package pavlosgi.freecli.core.dsl

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Tupler

import pavlosgi.freecli.core.api.Description
import pavlosgi.freecli.core.api.config.{Algebra, ArgumentName, StringDecoder}

package object config extends ConfigDslImplicits {
  import OptDslBuilder.DefaultValue

  type ConfigDsl[A] = FreeApplicative[Algebra, A]

  def config[T]: Apply[T] = new Apply[T]
  def config[T <: HList, Tup](
    c: ConfigDsl[T])
   (implicit ev: Tupler.Aux[T, Tup]):
    ConfigDsl[Tup] = {

    c.map(ev.apply)
  }

  def arg[T](implicit ev: StringDecoder[T]) = ArgumentsDslBuilder.arg[T]

  def string = arg[String]
  def int = arg[Int]
  def boolean = arg[Boolean]
  def name(name: String): ArgumentName = ArgumentName(name)


  def optString = opt[String]
  def optInt = opt[Int]
  def optBoolean = opt[Int]
  def flag(implicit ev: StringDecoder[Boolean]) = FlagDslBuilder.flag
  def req[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T] -~ required
  def opt[T](implicit ev: StringDecoder[T]) = OptDslBuilder.opt[T]
  def sub[T] = SubDslBuilder.sub[T]
  def sub[T](description: String) =
    SubDslBuilder.sub[Description :: HNil, T](Description(description) :: HNil)

  def required = OptDslBuilder.required
  def or[T](default: T): DefaultValue[T] = DefaultValue(default)
  def des(description: String): Description = Description(description)

  object o {
    def string = req[String]
    def int = req[Int]
    def boolean = req[Boolean]
  }
}