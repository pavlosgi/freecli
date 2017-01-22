package pavlosgi.freecli.command.api

import pavlosgi.freecli.config.{api => C}
import pavlosgi.freecli.core.free.FreeAlternative

sealed trait Action {
  def run(): Unit
}

case class ConfigAction[A](
  dsl: FreeAlternative[Algebra, A],
  f: FreeAlternative[Algebra, A] => String,
  action: C.Action)
  extends Action {

  def run(): Unit = {
    action match {
      case a@C.ArgumentAction(_) => a.run
      case o@C.OptionAction(_) => o.run(f(dsl))
    }
  }
}
