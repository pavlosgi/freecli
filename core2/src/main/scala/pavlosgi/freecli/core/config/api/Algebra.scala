package pavlosgi.freecli.core.api.config

import cats.Applicative

trait Algebra[F[_]] extends Applicative[F] {
  def arg[T, A](
    field: Field,
    f: T => A,
    g: StringDecoder[T],
    default: Option[T]):
    F[A]

  def opt[T, A](
    field: Field,
    f: Option[T] => A,
    g: StringDecoder[T]):
    F[A]

  def flag[A](
    field: Field,
    f: Boolean => A,
    default: Option[Boolean]):
    F[A]

  def sub[A, G[_]](
    description: Description,
    f: G[A])
   (implicit ev: G[A] => F[A]):
    F[A]
}



