package freecli
package config

import cats.free.FreeApplicative

import api.Algebra

package object dsl {
  type ConfigDsl[A] = FreeApplicative[Algebra, A]
}



