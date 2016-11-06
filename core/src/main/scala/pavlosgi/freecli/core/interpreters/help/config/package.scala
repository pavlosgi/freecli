package pavlosgi.freecli.core.interpreters.help

import cats.data._
import cats.syntax.all._

import pavlosgi.freecli.core.api.config._

package object config {

  type Result[_] = State[HelpState, Unit]

  def configHelp[G, A]
    (dsl: G)
    (implicit ev: G => Result[A]):
     String = {

    "Usage".bold.underline.newlineLeft.newline.newline +
      ev(dsl).runS(HelpState(2, "")).value.text.newline
  }

  implicit object configAlgebraHelp extends Algebra[Result] {

    def genHelp(text: String): Result[Unit] = {
      for {
        helpState <- State.get[HelpState]
        space = (0 until helpState.indentation)
                  .foldLeft[String]("")((a, _) => a + " ")

        _ <- State.set(HelpState(
              helpState.indentation,
              text = helpState.text + space + text.newline))

      } yield ()
    }

    def genSubHelp(description: Description): Result[Unit] = {
      for {
        helpState <- State.get[HelpState]
        _ <- State.set(HelpState(helpState.indentation, helpState.text + "\n"))
        _ <- genHelp(description.show.cyan)
      } yield ()
    }

    def genFieldHelp(field: Field): Result[Unit] = {
      genHelp(field match {
        case FieldNameOnly(name, description) =>
          String.format("%-15s   %s", name.show, description.fold("")(_.show))

        case FieldAbbreviationOnly(abbr, description) =>
          String.format("%-15s   %s", abbr.show, description.fold("")(_.show))

        case FieldNameAndAbbreviation(name, abbr, description) =>
          String.format(
            "%-15s   %s",
            name.show +", " + abbr.show,
            description.fold("")(_.show))
      })
    }

    override def arg[T, A](
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

    override def flag[A]
      (field: Field,
       f: Boolean => A,
       default: Option[Boolean]):
       Result[A] = genFieldHelp(field)

    override def sub[G, A]
      (description: Description,
       dsl: G)
      (implicit ev: G => Result[A]):
       Result[A] =

        for {
          _ <- genSubHelp(description)
          state <- State.get[HelpState]
          _ <- State.set(HelpState(state.indentation, state.text))
          _ <- ev(dsl)
          newState <- State.get[HelpState]
          _ <- State.set(HelpState(state.indentation, newState.text))
        } yield ()


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

  }
}