package pavlosgi.freecli.config

import cats.free.FreeApplicative

import pavlosgi.freecli.config.api.Algebra

package object dsl {
  type ConfigDsl[A] = FreeApplicative[Algebra, A]
}



