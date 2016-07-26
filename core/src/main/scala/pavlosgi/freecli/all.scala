package pavlosgi
package freecli

object all
  extends commands.Operations
  with commands.Instances
  with commands.Types
  with config.dsl.Operations
  with config.Instances {

  val Config = pavlosgi.freecli.config.all

}