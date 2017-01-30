package freecli
package argument
package api

sealed trait Action
case object NoOp extends Action {
  def run(): Unit = ()
}