package pavlosgi.freecli.core.dsl.command

import pavlosgi.freecli.core.dsl.CanProduce

trait CommandDslImplicits extends CommandFieldImplicits {

  implicit def toCommandDsl[B, Command](
    b: B)
   (implicit ev: CanProduce.Aux[B, CommandDsl[Command]]):
    CommandDsl[Command] = {
    ev(b)
  }

  implicit def canProduceCommandDslId[O] = CanProduce.Id[CommandDsl[O]]
}
