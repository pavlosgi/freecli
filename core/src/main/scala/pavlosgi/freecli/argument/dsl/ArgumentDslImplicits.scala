package pavlosgi.freecli.argument.dsl

import pavlosgi.freecli.core.ops.CanProduce

trait ArgumentDslImplicits extends ArgumentFieldImplicits with MergerImplicits {
  implicit def toArgumentDsl[B, A](
    b: B)
   (implicit ev: CanProduce.Aux[B, ArgumentDsl[A]]):
    ArgumentDsl[A] = {

    ev(b)
  }

  implicit def canProduceArgumentDslId[A]:
    CanProduce.Aux[ArgumentDsl[A], ArgumentDsl[A]] = {

    CanProduce.Id[ArgumentDsl[A]]
  }
}