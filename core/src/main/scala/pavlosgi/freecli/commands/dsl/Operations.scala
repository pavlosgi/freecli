package pavlosgi
package freecli
package commands
package dsl

import shapeless._

import pavlosgi.freecli.config.algebra.Plugin
import pavlosgi.freecli.config.dsl.ConfigDsl

trait Operations {
  def cmd(cmdName: String): NameAdded = {
    NameAdded(cmdName)
  }

  def description(description: String): DescriptionAdded = {
    DescriptionAdded(description)
  }

  def subcommand[E <: HList](events: Events[E]): CommandAdded[E] = {
    CommandAdded(events)
  }

  def config[G[_]: Plugin, H[_]: Plugin, A]
    (config: ConfigDsl[G, A]): ConfigAdded[G, A] = {

    ConfigAdded(config)
  }

  def run(run: () => Unit): RunAdded = {
    RunAdded(run)
  }

  def run[A](run: A => Unit): ConfigRunAdded[A] = {
    ConfigRunAdded(run)
  }
}

