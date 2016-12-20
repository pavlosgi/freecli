package pavlosgi.freecli.argument.dsl

import java.io.File

import pavlosgi.freecli.argument.api.ArgumentFieldName
import pavlosgi.freecli.core.{Description, ExistentFile, Grouping, NewFile, StringDecoder}

trait Ops extends Grouping {
  def arg[T](implicit ev: StringDecoder[T]) = ArgumentDslBuilder.arg[T]

  def string = arg[String]
  def int = arg[Int]
  def long = arg[Long]
  def double = arg[Double]
  def boolean = arg[Boolean]
  def file = arg[File]
  def existentFile = arg[ExistentFile]
  def newFile = arg[NewFile]
  def name(name: String): ArgumentFieldName = ArgumentFieldName(name)
  def des(description: String): Description = Description(description)
}
