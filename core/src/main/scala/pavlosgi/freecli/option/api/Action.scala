package pavlosgi.freecli.option.api

trait Action
case object HelpAction extends Action {
  def run(s: String): Unit = {
    println(s)
    sys.exit(0)
  }
}

case class VersionAction(v: StringValue) extends Action {
  def run(): Unit = {
    println(v.value)
    sys.exit(0)
  }
}
