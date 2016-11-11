package pavlosgi.freecli.core.api.arguments

import cats.Applicative

trait Algebra[F[_]] extends Applicative[F] {
  def arg[T, A](
    details: ArgumentDetails,
    f: T => A,
    g: StringDecoder[T]):
    F[A]
}



