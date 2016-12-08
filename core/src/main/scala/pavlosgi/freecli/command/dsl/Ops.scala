package pavlosgi.freecli.command.dsl

import shapeless.{::, HNil}

import pavlosgi.freecli.command.api.{CommandFieldName, RunCommand}
import pavlosgi.freecli.config.dsl._
import pavlosgi.freecli.core.Description

trait Ops {
  def cmd(name: String) =
    CommandDslBuilder[CommandFieldName :: HNil, Unit, Unit](
      CommandFieldName(name) :: HNil)

  def cmd(name: String, description: String) =
    CommandDslBuilder[CommandFieldName :: Description :: HNil, Unit, Unit](
      CommandFieldName(name) :: Description(description) :: HNil)

  def takes[T](c: ConfigDsl[T]): CommandDslBuilder[ConfigDsl[T] :: HNil, T, Unit] = {
    CommandDslBuilder(c :: HNil)
  }

  def runs[T](f: T => Unit) =
   CommandDslBuilder[RunCommand[T] :: HNil, Unit, T](
    RunCommand[T](f) :: HNil)

  def runs(f: => Unit) =
    CommandDslBuilder[RunCommand[Unit] :: HNil, Unit, Unit](
      RunCommand[Unit](_ => f) :: HNil)
}
