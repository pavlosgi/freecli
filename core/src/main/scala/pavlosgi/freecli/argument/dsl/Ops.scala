package pavlosgi.freecli.argument.dsl

import pavlosgi.freecli.argument.api.ArgumentName
import pavlosgi.freecli.core.{Description, Grouping, StringDecoder}

trait Ops extends Grouping {
  def arg[T](implicit ev: StringDecoder[T]) = ArgumentDslBuilder.arg[T]

  def string = arg[String]
  def int = arg[Int]
  def boolean = arg[Boolean]
  def name(name: String): ArgumentName = ArgumentName(name)
  def des(description: String): Description = Description(description)
}
