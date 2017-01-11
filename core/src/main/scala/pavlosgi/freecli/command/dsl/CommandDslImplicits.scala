package pavlosgi.freecli.command.dsl

import pavlosgi.freecli.core.api.CanProduce

trait CommandDslImplicits {
  implicit def toCommandDsl[B, T](
    b: B)
   (implicit ev: CanProduce.Aux[B, CommandDsl[T]]):
    CommandDsl[T] = {
    ev(b)
  }

  implicit def canProduceCommandDslId[O]: CanProduce[CommandDsl[O]] =
    CanProduce.Id[CommandDsl[O]]
}
