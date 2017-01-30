package freecli
package command

import api._
import core.free.FreeAlternative

package object dsl {
  type CommandDsl[A] = FreeAlternative[Algebra, A]
}
