package pavlosgi.freecli.core.dsl.arguments

import pavlosgi.freecli.core.dsl.CanProduce

trait ArgumentsDslImplicits extends ArgumentsDetailsImplicits {
  implicit def toArgumentDslMerger[B, A](
    b: B)
   (implicit ev: CanProduce.Aux[B, ArgumentsDsl[A]]):
    ArgumentDslMerger[A] = {

    new ArgumentDslMerger[A](ev(b))
  }

  implicit def toArgumentDsl[B, A](
    b: B)
   (implicit ev: CanProduce.Aux[B, ArgumentsDsl[A]]):
    ArgumentsDsl[A] = {

    ev(b)
  }

  implicit def canProduceArgumentsDslId[O] = CanProduce.Id[ArgumentsDsl[O]]
}