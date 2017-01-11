package pavlosgi.freecli.config

import pavlosgi.freecli.config.dsl.{ConfigDslImplicits, MergerImplicits}
import pavlosgi.freecli.{argument => A}
import pavlosgi.freecli.{option => O}

object implicits extends AllImplicits
trait AllImplicits
  extends ConfigDslImplicits
  with MergerImplicits
  with A.AllImplicits
  with O.AllImplicits