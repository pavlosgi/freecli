package pavlosgi.freecli.arguments.dsl

import pavlosgi.freecli.core.CanProduce

trait ArgumentsDslImplicits extends ArgumentsDetailsImplicits with MergerImplicits {
  implicit def toArgumentDsl[B, A](
    b: B)
   (implicit ev: CanProduce.Aux[B, ArgumentsDsl[A]]):
    ArgumentsDsl[A] = {

    ev(b)
  }

  implicit def canProduceArgumentsDslId[O] = CanProduce.Id[ArgumentsDsl[O]]
}