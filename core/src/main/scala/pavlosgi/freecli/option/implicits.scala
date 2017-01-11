package pavlosgi.freecli.option

import pavlosgi.freecli.option.dsl._

object implicits extends AllImplicits
trait AllImplicits
  extends OptionDslImplicits
  with DefaultImplicits
  with OptionFieldImplicits
  with DescriptionImplicits
  with MergerImplicits