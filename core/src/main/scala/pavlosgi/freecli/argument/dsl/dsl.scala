package pavlosgi.freecli.argument

import cats.free.FreeApplicative

import pavlosgi.freecli.argument.api.Algebra

package object dsl {
  type ArgumentDsl[A] = FreeApplicative[Algebra, A]
}