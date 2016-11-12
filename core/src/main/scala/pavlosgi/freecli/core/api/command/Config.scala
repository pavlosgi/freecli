package pavlosgi.freecli.core.api.command

import cats.~>

import pavlosgi.freecli.core.api.config.{Algebra => ConfigAlgebra}

trait Config[F[_], C[_]] {
  def configAlgebra: ConfigAlgebra[C]
  def nat: C ~> F
}
