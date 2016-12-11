package pavlosgi.freecli.core

case class Marked[A](value: A, isMarked: Boolean)
object Marked {
  def unmarked[A](v: A) = Marked(v, false)
}