package freecli
package config

import dsl.{ConfigDslImplicits, MergerImplicits}
import freecli.{argument => A}
import freecli.{option => O}

object implicits extends AllImplicits
trait AllImplicits
  extends ConfigDslImplicits
  with MergerImplicits
  with A.AllImplicits
  with O.AllImplicits