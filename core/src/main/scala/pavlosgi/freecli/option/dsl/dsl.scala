package pavlosgi.freecli.option

import cats.free.FreeApplicative

import pavlosgi.freecli.option.api.Algebra

package object dsl {
  type OptionDsl[A] = FreeApplicative[Algebra, A]
}

