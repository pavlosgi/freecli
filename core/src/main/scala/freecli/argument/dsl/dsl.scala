package freecli
package argument

import cats.free.FreeApplicative

import api.Algebra

package object dsl {
  type ArgumentDsl[A] = FreeApplicative[Algebra, A]
}