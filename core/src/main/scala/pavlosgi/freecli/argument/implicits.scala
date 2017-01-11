package pavlosgi.freecli.argument

import pavlosgi.freecli.argument.dsl._

object implicits extends AllImplicits
trait AllImplicits
  extends ArgumentDslImplicits
  with ArgumentFieldImplicits
  with MergerImplicits