package freecli
package argument

import dsl._

object implicits extends AllImplicits
trait AllImplicits
  extends ArgumentDslImplicits
  with ArgumentFieldImplicits
  with MergerImplicits