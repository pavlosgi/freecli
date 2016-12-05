package pavlosgi.freecli.core.dsl

import cats.free.FreeApplicative

import pavlosgi.freecli.core.api.{Description, StringDecoder}
import pavlosgi.freecli.core.api.arguments.{Algebra, ArgumentName}

package object arguments extends ArgumentsDslImplicits with TransformationOps {
  type ArgumentsDsl[A] = FreeApplicative[Algebra, A]

  def arg[T](implicit ev: StringDecoder[T]) = ArgumentsDslBuilder.arg[T]

  def string = arg[String]
  def int = arg[Int]
  def boolean = arg[Boolean]
  def name(name: String): ArgumentName = ArgumentName(name)
  def des(description: String): Description = Description(description)
}