package pavlosgi.freecli.core.api

import cats.~>

trait AlgebraDependency[G[_[_]], F[_], C[_]] {
  def algebra: G[C]
  def nat: C ~> F
}
