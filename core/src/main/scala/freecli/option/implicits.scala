package freecli
package option

import option.dsl._

object implicits extends AllImplicits
trait AllImplicits
  extends OptionDslImplicits
  with DefaultImplicits
  with OptionFieldImplicits
  with StringValueImplicits
  with DescriptionImplicits
  with MergerImplicits