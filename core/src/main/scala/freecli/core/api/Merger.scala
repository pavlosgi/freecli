package freecli
package core
package api

case class Merger[F](f: F) {
  def ::[F2](merger: Merger[F2])(implicit ev: CanMerge[F2, F]): ev.Out = {
    ev(merger.f, f)
  }
}

trait CanMerge[F1, F2] {
  type Out
  def apply(f1: F1, f2: F2): Out
}

object CanMerge {
  type Aux[F1, F2, Out0] = CanMerge[F1, F2] { type Out = Out0 }
}
