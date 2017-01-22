package pavlosgi.freecli.argument.api

sealed trait Action
case object NoOp extends Action {
  def run(): Unit = ()
}