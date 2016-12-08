package pavlosgi.freecli.config

import cats.free.FreeApplicative

import pavlosgi.freecli.arguments.{dsl => A}
import pavlosgi.freecli.config.api.Algebra
import pavlosgi.freecli.core.{Description, StringDecoder, Grouping}
import pavlosgi.freecli.options.{dsl => O}

package object dsl extends ConfigDslImplicits with Grouping {
  type ConfigDsl[A] = FreeApplicative[Algebra, A]

  def arg[T](implicit ev: StringDecoder[T]) = A.arg[T]

  def string = A.string
  def int = A.int
  def boolean = A.boolean
  def name(name: String) = A.name(name)

  def flag(implicit ev: StringDecoder[Boolean]) = O.flag(ev)
  def req[T](implicit ev: StringDecoder[T]) = O.req(ev)
  def opt[T](implicit ev: StringDecoder[T]) = O.opt(ev)
  def sub[T](description: String) = O.sub[T](description)
  def subT(description: String) = O.subT(description)

  def required = O.required
  def or[T](default: T) = O.or(default)

  def des(description: String): Description = Description(description)

  object o {
    def string = O.string
    def int = O.int
    def boolean = O.boolean
  }
}



