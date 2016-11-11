package pavlosgi.freecli.core.dsl

import shapeless._

import pavlosgi.freecli.core.api.command._
import pavlosgi.freecli.core.dsl.options.OptionsDsl

package object command {
  def cmd(name: String) =
    CommandPartsBuilder[CommandFieldName :: HNil, Unit, Unit](
      CommandFieldName(name) :: HNil)

  def cmd(name: String, description: String) =
    CommandPartsBuilder[CommandFieldName :: Description :: HNil, Unit, Unit](
      CommandFieldName(name) :: Description(description) :: HNil)

  implicit def optDslToCommandPartsBuilder[T](
    c: OptionsDsl[T]):
    CommandPartsBuilder[OptionsDsl[T] :: HNil, T, Unit] = {

    CommandPartsBuilder(c :: HNil)
  }

  def runs[T](f: T => Unit) =
   CommandPartsBuilder[RunCommand[T] :: HNil, Unit, T](
    RunCommand[T](f) :: HNil)

  def runs(f: => Unit) =
    CommandPartsBuilder[RunCommand[Unit] :: HNil, Unit, Unit](
      RunCommand[Unit](_ => f) :: HNil)
}
