package pavlosgi.freecli.core.free

import cats.arrow.FunctionK
import cats.data.Const
import cats.{Alternative, Applicative, Monoid}

sealed abstract class FreeAlternative[F[_], A] extends Product with Serializable { self =>
  import FreeAlternative.{Ap, CombineK, Empty, FA, Pure, lift, ap => apply}

  final def ap[B](b: FA[F, A => B]): FA[F, B] =
    b match {
      case Pure(f) =>
        this.map(f)

      case Ap(pivot, fn) =>
        apply(pivot)(self.ap(fn.map(fx => a => p => fx(p)(a))))

      case CombineK(f1, f2) =>
        self.ap(f1.combineK(f2))

      case Empty() =>
        Empty()
    }

  final def combineK[B](b: FA[F, A]): FA[F, A] =
    b match {
      case Pure(f) =>
        CombineK(self, Pure(f))

      case Ap(pivot, fn) =>
        CombineK(self, Ap(pivot, fn))

      case CombineK(f1, f2) =>
        CombineK(self, CombineK(f1, f2))

      case Empty() =>
        self
    }

  final def empty: FA[F, A] = Empty()

  final def map[B](f: A => B): FA[F, B] =
    this match {
      case Pure(a) => Pure(f(a))
      case Ap(pivot, fn) => apply(pivot)(fn.map(f compose _))
      case CombineK(f1, f2) => CombineK(f1.map(f), f2.map(f))
      case Empty() => Empty()
    }

  final def foldMap[G[_]](f: FunctionK[F, G])(implicit G: Alternative[G]): G[A] =
    this match {
      case Pure(a) => G.pure(a)
      case Ap(pivot, fn) => G.map2(f(pivot), fn.foldMap(f))((a, g) => g(a))
      case CombineK(f1, f2) => G.combineK(f1.foldMap(f), f2.foldMap(f))
      case Empty() => G.empty
    }

  final def fold(implicit F: Alternative[F]): F[A] =
    foldMap(FunctionK.id[F])

  final def compile[G[_]](f: FunctionK[F, G]): FA[G, A] =
    foldMap[FA[G, ?]] {
      new FunctionK[F, FA[G, ?]] {
        def apply[B](fa: F[B]): FA[G, B] = lift(f(fa))
      }
    }

  final def analyze[M: Monoid](f: FunctionK[F, Lambda[α => M]]): M = {
    implicit val constAlternative = new Alternative[Const[M, ?]] {
      def empty[T]: Const[M, T] =
        implicitly[Monoid[Const[M, T]]].empty

      def combineK[T](x: Const[M, T], y: Const[M, T]): Const[M, T] = {
        Const.apply(implicitly[Monoid[M]].combine(x.getConst, y.getConst))
      }

      def pure[T](x: T): Const[M, T] = {
        implicitly[Applicative[Const[M, ?]]].pure(x)
      }

      def ap[T, B](ff: Const[M, T => B])(fa: Const[M, T]): Const[M, B] = {
        implicitly[Applicative[Const[M, ?]]].ap(ff)(fa)
      }
    }

    foldMap[Const[M, ?]](new (FunctionK[F, Const[M, ?]]) {
      def apply[X](x: F[X]): Const[M, X] = Const(f(x))
    }).getConst
  }

  override def toString: String = "FreeAlternative(...)"
}

object FreeAlternative {
  type FA[F[_], A] = FreeAlternative[F, A]

  private final case class Pure[F[_], A](a: A) extends FA[F, A]

  private final case class Ap[F[_], P, A](pivot: F[P], fn: FA[F, P => A])
    extends FA[F, A]

  private final case class CombineK[F[_], A](f1: FA[F, A], f2: FA[F, A])
    extends FA[F, A]

  private final case class Empty[F[_], A]() extends FA[F, A]

  final def pure[F[_], A](a: A): FA[F, A] =
    Pure(a)

  final def ap[F[_], P, A](fp: F[P])(f: FA[F, P => A]): FA[F, A] = Ap(fp, f)

  final def combineK[F[_], A](f1: FA[F, A], f2: FA[F, A]): FA[F, A] =
    CombineK(f1, f2)

  final def empty[F[_], A]: FA[F, A] = Empty()

  final def lift[F[_], A](fa: F[A]): FA[F, A] =
    ap(fa)(Pure(a => a))

  implicit final def freeAlternative[S[_]]: Alternative[FA[S, ?]] = {
    new Alternative[FA[S, ?]] {
      override def product[A, B](fa: FA[S, A], fb: FA[S, B]): FA[S, (A, B)] =
        ap(fa.map((a: A) => (b: B) => (a, b)))(fb)

      override def map[A, B](fa: FA[S, A])(f: A => B): FA[S, B] = fa.map(f)
      override def ap[A, B](f: FA[S, A => B])(fa: FA[S, A]): FA[S, B] = fa.ap(f)
      def pure[A](a: A): FA[S, A] = Pure(a)

      def combineK[A](f1: FA[S, A], f2: FA[S, A]): FA[S, A] =
        f1.combineK(f2)

      def empty[A]: FA[S, A] = Empty()
    }
  }
}
