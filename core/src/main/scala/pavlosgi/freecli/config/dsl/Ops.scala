package pavlosgi.freecli.config.dsl

import pavlosgi.freecli.{argument => A}
import pavlosgi.freecli.core.{Description, Grouping, StringDecoder}
import pavlosgi.freecli.{option => O}

trait Ops extends Grouping {
  def arg[T](implicit ev: StringDecoder[T]) = A.arg[T]

  def string = A.string
  def int = A.int
  def long = A.long
  def double = A.double
  def boolean = A.boolean
  def file = A.file
  def existentFile = A.existentFile
  def newFile = A.newFile
  def name(name: String) = A.name(name)

  def flag(implicit ev: StringDecoder[Boolean]) = O.flag(ev)
  def opt[T](implicit ev: StringDecoder[T]) = O.opt(ev)
  def sub[T](description: String) = O.sub[T](description)
  def subT(description: String) = O.subT(description)

  def req = O.req
  def or[T](default: T) = O.or(default)

  def des(description: String): Description = Description(description)

  object o {
    def string = O.string
    def int = O.int
    def long = O.long
    def double = O.double
    def boolean = O.boolean
    def file = O.file
    def existentFile = O.existentFile
    def newFile = O.newFile
  }
}
