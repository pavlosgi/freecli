package pavlosgi.freecli.core.dsl.arguments

import cats.free.FreeApplicative
import shapeless._
import shapeless.ops.hlist.Prepend

import pavlosgi.freecli.core.api.{Description, StringDecoder}
import pavlosgi.freecli.core.api.arguments._
import pavlosgi.freecli.core.dsl.CanProduce

case class ArgumentsDslBuilder[H <: HList, T](list: H) {
  def -~(
    description: Description)
   (implicit ev: Prepend[H, Description :: HNil],
    ev2: NotContainsConstraint[H, Description]) =

    new ArgumentsDslBuilder[ev.Out, T](list :+ description)

  def -~(
    name: ArgumentName)
   (implicit ev: Prepend[H, ArgumentName :: HNil],
    ev2: NotContainsConstraint[H, ArgumentName]) =

    new ArgumentsDslBuilder[ev.Out, T](list :+ name)
}

object ArgumentsDslBuilder {
  def arg[T](implicit decoder: StringDecoder[T]): ArgumentsDslBuilder[HNil, T] =
    arg(HNil)

  def arg[H <: HList, T](list: H)(implicit decoder: StringDecoder[T]) =
    new ArgumentsDslBuilder[H, T](list)

  implicit def canProduceArgDsl[H <: HList, T, Out <: HList](
    implicit canProduceArgumentDetails: CanProduce.Aux[H, (ArgumentDetails, HNil)],
    decoder: StringDecoder[T]):
    CanProduce.Aux[ArgumentsDslBuilder[H, T], ArgumentsDsl[T]] = {

    new CanProduce[ArgumentsDslBuilder[H, T]] {
      type Out = ArgumentsDsl[T]
      def apply(t: ArgumentsDslBuilder[H, T]): Out = {
        val (ad, _) = canProduceArgumentDetails.apply(t.list)
        FreeApplicative.lift(Arg[T, T](ad, identity, decoder))
      }
    }
  }

//  implicit def functorInstance[H <: HList, T](
//    implicit ev: CanProduce.Aux[ArgumentsDslBuilder[H, T], ArgumentsDsl[T]]) =  {
//    new Functor[ArgumentsDslBuilder[H, ?]] {
//      def map[A, B](fa: ArgumentsDslBuilder[H, A])(f: A => B): ArgumentsDslBuilder[H, B] = {
//        ArgumentsDslBuilder(ev(fa).map(f) :: HNil)
//      }
//    }
//  }
}