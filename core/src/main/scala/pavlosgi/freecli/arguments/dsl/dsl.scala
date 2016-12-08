package pavlosgi.freecli.arguments

import cats.free.FreeApplicative

import pavlosgi.freecli.arguments.api.{Algebra, ArgumentName}
import pavlosgi.freecli.core.{Description, StringDecoder, Grouping}

package object dsl extends ArgumentsDslImplicits with Grouping {
  type ArgumentsDsl[A] = FreeApplicative[Algebra, A]

  def arg[T](implicit ev: StringDecoder[T]) = ArgumentsDslBuilder.arg[T]

  def string = arg[String]
  def int = arg[Int]
  def boolean = arg[Boolean]
  def name(name: String): ArgumentName = ArgumentName(name)
  def des(description: String): Description = Description(description)
}