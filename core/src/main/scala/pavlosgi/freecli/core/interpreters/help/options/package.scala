package pavlosgi.freecli.core.interpreters.help

import cats.data._
import cats.syntax.all._

import pavlosgi.freecli.core.api.options._

package object options {

  def optionsHelp[G, A](
    dsl: G)
   (implicit ev: G => Result[A]):
    String = {

    (for {
      _ <- Result.newline
      _ <- Result.append("Usage".bold.underline)
      _ <- Result.newline
      _ <- Result.newline
      _ <- ev(dsl)
      _ <- Result.newline
    } yield ()).runS(HelpState(2, "")).value.text
  }

  implicit object configAlgebraHelp extends Algebra[Result] {
    override def requiredOpt[T, A](
      field: Field,
      f: T => A,
      g: StringDecoder[T],
      default: Option[T]):
      Result[A] = genFieldHelp(field)

    override def opt[T, A](
      field: Field,
      f: Option[T] => A,
      g: StringDecoder[T]):
      Result[A] = genFieldHelp(field)

    override def flag[A](
      field: Field,
      f: Boolean => A):
      Result[A] = genFieldHelp(field)

    override def sub[G, A](
      description: Description,
      dsl: G)
     (implicit ev: G => Result[A]):
      Result[A] = {

      for {
         _ <- genSubHelp(description)
         _ <- ev(dsl)
      } yield ()

    }

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

    def genSubHelp(description: Description): Result[Unit] = {
      for {
        _ <- Result.newline
        _ <- Result.appendAtIndentationLn(description.show)
      } yield ()
    }

    def genFieldHelp(field: Field): Result[Unit] = {
      Result.appendAtIndentationLn(field match {
        case FieldNameOnly(name, description) =>
          String.format("%-15s   %s", name.show.yellow, description.fold("")(_.show))

        case FieldAbbreviationOnly(abbr, description) =>
          String.format("%-15s   %s", abbr.show.yellow, description.fold("")(_.show))

        case FieldNameAndAbbreviation(name, abbr, description) =>
          String.format(
            "%-15s   %s",
            s"${name.show.yellow}, ${abbr.show.yellow}",
            description.fold("")(_.show))
      })
    }
  }
}