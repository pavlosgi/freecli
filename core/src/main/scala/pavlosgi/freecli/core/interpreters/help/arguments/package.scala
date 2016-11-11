package pavlosgi.freecli.core.interpreters.help

import cats.data._

import pavlosgi.freecli.core.api.arguments._

package object arguments {

  def argumentsHelp[G, A](
    dsl: G)
   (implicit ev: G => Result[A]):
    String = {

    (for {
      _ <- Result.newline
      _ <- Result.append("Usage".bold.underline)
      _ <- Result.newline
      _ <- Result.newline
      _ <- Result.appendAtIndentation("Program")
      _ <- ev(dsl)
      _ <- Result.newline
      _ <- Result.newline
    } yield ()).runS(HelpState(2, "")).value.text
  }

  implicit object argAlgebraHelp extends Algebra[Result] {
    override def arg[T, A](
      details: ArgumentDetails,
      f: T => A,
      g: StringDecoder[T]):
      Result[A] = getArgumentHelp(details)

    override def pure[A](x: A): Result[A] = State.pure(())

    override def ap[A, B]
      (ff: Result[A => B])
      (fa: Result[A]):
       Result[B] = {

      for {
        ff1 <- ff.get
        fa1 <- fa.get
      } yield ()
    }

    def getArgumentHelp(details: ArgumentDetails): Result[Unit] = {
      Result.append(s" <${details.placeholder.value}>")
    }
  }
}