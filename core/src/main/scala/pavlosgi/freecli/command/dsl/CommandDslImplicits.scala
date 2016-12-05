package pavlosgi.freecli.command.dsl

import pavlosgi.freecli.core.CanProduce

trait CommandDslImplicits extends CommandFieldImplicits with CommandDslBuilderImplicits {

  implicit def toCommandDsl[B, T](
    b: B)
   (implicit ev: CanProduce.Aux[B, CommandDsl[T]]):
    CommandDsl[T] = {
    ev(b)
  }

  implicit def canProduceCommandDslId[O] = CanProduce.Id[CommandDsl[O]]
}
