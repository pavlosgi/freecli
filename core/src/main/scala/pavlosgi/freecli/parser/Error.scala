package pavlosgi.freecli.parser

trait Error[E] {
  def message(error: E): String
}
