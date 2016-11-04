package pavlosgi
package freecli
package commands
package dsl

import algebra.ApplyCommandAlgebra

trait Types {
  type CommandsDsl[G[_], A] = ApplyCommandAlgebra[G, A]
}
