package pavlosgi.freecli.command

import shapeless._

import pavlosgi.freecli.config.dsl.ConfigDsl
import pavlosgi.freecli.free.FreeAlternative
import pavlosgi.freecli.command.api._
import pavlosgi.freecli.core.Description

package object dsl extends CommandDslImplicits {
  type CommandDsl[A] = FreeAlternative[Algebra, A]

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
