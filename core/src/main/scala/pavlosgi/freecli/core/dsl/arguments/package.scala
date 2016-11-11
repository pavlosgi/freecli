package pavlosgi.freecli.core.dsl

import cats.syntax.all._
import shapeless._
import shapeless.ops.hlist.Tupler

import pavlosgi.freecli.core.api.arguments.{Description, Placeholder, StringDecoder}

package object arguments {
  def arguments[T]: ArgDsl.Apply[T] = new ArgDsl.Apply[T]
  def arguments[T <: HList, Tup](
    c: ArgDsl[T])
   (implicit ev: Tupler.Aux[T, Tup]):
    ArgDsl[Tup] = c.map(ev.apply)

  def arg[T](implicit ev: StringDecoder[T]) = ArgDslBuilder.arg[T]

  def string = arg[String]
  def int = arg[Int]
  def boolean = arg[Boolean]
  def des(description: String): Description = Description(description)
  def name(placeholder: String): Placeholder = Placeholder(placeholder)
}
