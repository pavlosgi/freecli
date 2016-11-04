package pavlosgi.freecli.core.command

import shapeless._

import api.CommandFieldName
import pavlosgi.freecli.core.command.dsl.PartialCommandDsl.{PartialCommandConfiguration, Subcommands}
import pavlosgi.freecli.core.dsl.config.ConfigDsl

package object dsl {

  def cmd(name: String) =
    new PartialCommandDsl[CommandFieldName :: HNil, Unit, Unit](
      CommandFieldName(name) :: HNil)

  def takes[T](config: ConfigDsl[T :: HNil]) =
    PartialCommandConfiguration.takes(config)

  def runs[T](f: T => Unit) =
    PartialCommandConfiguration.runs(f)

  def runs(f: => Unit) =
    PartialCommandConfiguration.runs(f)

  def subcommands[T] = Subcommands.apply[T]
}