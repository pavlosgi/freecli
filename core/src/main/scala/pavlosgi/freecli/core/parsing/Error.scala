package pavlosgi.freecli.core.parsing

trait Error[E] {
  def message(error: E): String
}
