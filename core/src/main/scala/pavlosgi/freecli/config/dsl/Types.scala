package pavlosgi
package freecli
package config
package dsl

import algebra.ApplyConfigAlgebra

trait Types {
  type ConfigDsl[G[_], A] = ApplyConfigAlgebra[G, A]
}
