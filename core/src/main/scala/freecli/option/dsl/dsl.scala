package freecli
package option

import cats.free.FreeApplicative

import option.api.Algebra

package object dsl {
  type OptionDsl[A] = FreeApplicative[Algebra, A]
}

