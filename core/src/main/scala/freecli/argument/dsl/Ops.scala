package freecli
package argument
package dsl

import java.io.File

import api.ArgumentFieldName
import core.api.{ExistentFile, NewFile, StringDecoder}

trait Ops {
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
}
