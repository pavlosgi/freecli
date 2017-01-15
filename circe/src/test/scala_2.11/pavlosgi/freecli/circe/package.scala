package pavlosgi.freecli

package object circe {
  implicit class EitherOps[E, A](e: Either[E, A]) {
    def toOption: Option[A] = e match {
      case Left(_)  => None
      case Right(a) => Some(a)
    }
  }

}