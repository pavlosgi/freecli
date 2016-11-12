package pavlosgi.freecli.core.api.config

import cats.Applicative

trait Algebra[F[_]] extends Applicative[F] {
  def arg[T, A](
    details: ArgumentDetails,
    f: T => A,
    g: StringDecoder[T]):
    F[A]

  def requiredOpt[T, A](
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
    f: Boolean => A):
    F[A]

  def sub[G, A](
    description: Description,
    f: G)
   (implicit ev: G => F[A]):
    F[A]
}