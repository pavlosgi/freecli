package pavlosgi.freecli.core.dsl

import cats.free.FreeApplicative

import pavlosgi.freecli.core.api.{Description, StringDecoder}
import pavlosgi.freecli.core.api.config.Algebra
import pavlosgi.freecli.core.dsl.{arguments => A, options => O}

package object config extends ConfigDslImplicits with TransformationOps {
  type ConfigDsl[A] = FreeApplicative[Algebra, A]

  def options[O](o: O.OptionsDsl[O]) = {
    ConfigDslBuilder.options(o)
  }

  def arguments[A](a: A.ArgumentsDsl[A]) = {
    ConfigDslBuilder.arguments(a)
  }

  def arg[T](implicit ev: StringDecoder[T]) = A.arg[T]

  def string = A.string
  def int = A.int
  def boolean = A.boolean
  def name(name: String) = A.name(name)

  def flag(implicit ev: StringDecoder[Boolean]) = O.flag(ev)
  def req[T](implicit ev: StringDecoder[T]) = O.req(ev)
  def opt[T](implicit ev: StringDecoder[T]) = O.opt(ev)
  def sub[T](description: String) = O.sub(description)

  def required = O.required
  def or[T](default: T) = O.or(default)

  def des(description: String): Description = Description(description)

  object o {
    def string = O.string
    def int = O.int
    def boolean = O.boolean
  }
}

