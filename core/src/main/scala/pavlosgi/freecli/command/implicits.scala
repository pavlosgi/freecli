package pavlosgi.freecli.command

import pavlosgi.freecli.command.dsl._

object implicits extends AllImplicits
trait AllImplicits
  extends CommandDslImplicits
  with CommandDslBuilderImplicits
  with CommandFieldImplicits
  with MergerImplicits
  with parser.Instances